import serial
import os
import sys
import subprocess
import serial.tools.list_ports
import pdb
import struct
import numpy as np
import pdb
import time

# btAddress = "CA:A1:E5:68:C7:DC"
btAddress="D0:7B:4A:C9:AB:01"

# i.e: python data_logger.py <fs>
if __name__ == '__main__':
  try:
    # Grab user input for serial port
    ports = serial.tools.list_ports.comports()

    i = 1
    ports = sorted(ports)
    for port, desc, hwid in ports:
      print("Serial device ({}) : {}".format(i, port))
      i += 1

    port = input("Select a device by its number:")

    try:
      port = int(port)
    except ValueError:
      print("Invalid port number")

    port -= 1 # offset by 1

    if (port >= len(ports)) or (port < 0):
      print("Invalid port number")
      exit

    portName = ports[port].device
    ser = serial.Serial(portName, 115200)

    # Create output file
    f = open("output.bin","w+")
    trailingSample = None

    ser.write("ts on\n")
    scriptIndex = 0
    while True:
      data = str(ser.readline())
      data = data.strip()
      if (scriptIndex == 0 and "Time sync started" in data):
        ser.write("scan on\n")
        time.sleep(.25)
        scriptIndex += 1
      if (scriptIndex == 1 and "Scan started" in data):
        ser.write("connect " + btAddress + " \n")
        time.sleep(.25)
        scriptIndex += 1
      if (scriptIndex == 2 and "Data length updated to 251 bytes" in data):
        ser.write("parameters phy 2M "+ btAddress + "\n")
        time.sleep(.25)
        scriptIndex += 1
      if (scriptIndex == 3 and "PHY set to 2 Mbps" in data):
        ser.write("gatt services " + btAddress + "\n")
        time.sleep(.25)
        scriptIndex += 1
      if (scriptIndex == 4 and "Found service UUIDs" in data):
        ser.write("gatt characteristics " + btAddress + " 1400\n")
        time.sleep(.25)
        scriptIndex += 1
      if (scriptIndex == 5 and "Number of characteristics" in data):
        ser.write("gatt notification on " + btAddress + " 1402\n")
        time.sleep(.25)
        scriptIndex += 1
      # pdb.set_trace())
      print(data)


    # Start streaming
    while True:
      data = ser.read(ser.in_waiting+2)

      # If there's a trailing sample prepend it to the data
      if (trailingSample):
        data.insert(0, trailingSample)

      for i in range (0, len(data), 2):
        if ((i+1) == len(data)):
          trailingSample = data[i]
          break

        trailingSample = None
        bits = str(data[i+1]) + str(data[i])
        sample = struct.unpack('>h', bits)[0]
        f.write(str(sample)+"\n")
        print(sample)

  # Cleanup
  except KeyboardInterrupt:
    ser.write("disconnect " + btAddress + "\n")
    ser.write("reset\n")
    ser.close()
    print("Exiting...")
    f.close()
