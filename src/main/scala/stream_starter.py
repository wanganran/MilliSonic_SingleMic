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

TS=True
btAddress="CA:A1:E5:68:C7:DC"
#btAddress="D0:7B:4A:C9:AB:01"
#btAddress="F8:0E:C5:1E:C2:26"
#btAddress="E2:6D:BF:ED:08:C7"
# btAddress="EB:D6:61:23:42:18"

prevCommand = ""

def serialHelper(port, command):
  prevCommand = command
  for _ in command:
    ser.write(_)
    time.sleep(0.001)
  ser.write("\n")

# i.e: python data_logger.py <fs>
if __name__ == '__main__':
  try:
    # # Grab user input for serial port
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
    # portName = "/dev/tty.usbmodem0006832759131"
    ser = serial.Serial(portName, 115200)

    # Create output file
    f = open("output.bin","w+")
    trailingSample = None
    if TS:
        serialHelper(ser, "ts on")
        scriptIndex = 0
    else:
        serialHelper(ser, "ts off")
        time.sleep(.25)
        scriptIndex = 0

    print("Sending commands...")

    while True:
      data = str(ser.readline())
      data = data.strip()
      if ("not found" in data):
        print("Error sending last command, retrying")
        serialHelper(ser, prevCommand)
        continue
      if (scriptIndex == 0 and "Time sync" in data):
        serialHelper(ser, "scan on")
        # time.sleep(.25)
        scriptIndex += 1
      if (scriptIndex == 1 and "Scan started" in data):
        serialHelper(ser, "connect " + btAddress + " ")
        # time.sleep(.25)
        scriptIndex += 1
      if (scriptIndex == 2 and "Data length updated to 251 bytes" in data):
        serialHelper(ser, "parameters phy 2M "+ btAddress + "")
        # time.sleep(.25)
        scriptIndex += 1
      if (scriptIndex == 3 and "PHY set to 2 Mbps" in data):
        serialHelper(ser, "gatt services " + btAddress + "")
        # time.sleep(.25)
        scriptIndex += 1
      if (scriptIndex == 4 and "Found service UUIDs" in data):
        serialHelper(ser, "gatt characteristics " + btAddress + " 1400")
        # time.sleep(.25)
        scriptIndex += 1
      if (scriptIndex == 5 and "Number of characteristics" in data):
        serialHelper(ser, "gatt notification on " + btAddress + " 1402")
        # time.sleep(.25)
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
