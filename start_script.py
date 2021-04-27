from gpiozero import Button
from time import sleep, time
import os.path
import subprocess

# keep monitor button

lastbtn=0
T=0.1

proc_connect='python MilliSonic_SingleMic/stream_starter.py /dev/ttyACM2 > stream_starter.log'

proc_ms='java -cp MilliSonic_SingleMic/target/scala-2.13/SingleMicMilliSonic-assembly-0.1.jar OfflineMain'

f_ap='ap.fifo'
f_spk='spk.fifo'
f_alert='alert.fifo'
f_log='ms.log'

ap='/dev/ttyACM0'
spk='/dev/ttyACM1'

proc_ap='nanocom -p '+ap
proc_spk='nanocom -p '+spk

proc_alert='python alert.py'

def popen(cmd, pipeout=False):
    if not pipeout:
        return subprocess.Popen(['exec '+cmd], shell=True)
    else:
        return subprocess.Popen(['exec '+cmd], shell=True, stdout=subprocess.PIPE)

MAX_RETRY=10
def next_file(prefix):
    i=0
    while i<MAX_RETRY:
        if not os.path.exists(prefix+str(i)):
            return prefix+str(i)
    return None

p_connect=None
p_ap=None
p_spk=None
p_alert=None
p_ms=None

def start_process(connect):
    global p_connect
    if connect:
        pconnect=popen(proc_connect)
        sleep(6)
    else:
        print("connected")
        pconnect=p_connect

    start_time=round(time())

    path_ap=next_file(str(start_time)+'_ap_')
    path_spk=next_file(str(start_time)+'_spk_')

    palert=proc_alert + ' < '+f_alert
    p_alert=popen(palert)

    sleep(0.5)

    pspk=proc_spk+' > '+f_spk
    p_spk=popen(pspk)

    pms=proc_ms+' '+f_ap+' '+f_spk+' '+f_alert+' '+path_ap+' '+path_spk #+' '+f_log
    p_ms=popen(pms)

    sleep(5)

    pap=proc_ap + ' > '+f_ap
    p_ap=popen(pap)

    return (pconnect, p_ap, p_spk, p_alert, p_ms)


def btn_pressed():
    global lastbtn
    global p_connect, p_ap, p_spk, p_alert, p_ms
    t=time()
    if t-lastbtn>T:
        print('pressed')
        (p_connect, p_ap, p_spk, p_alert, p_ms)=start_process(p_connect is None)
        
    lastbtn=t

def btn_released():
    global lastbtn
    global p_ap, p_spk, p_alert, p_ms
    t=time()
    if t-lastbtn>T:
        print('released')
        p_ap.terminate()
        p_spk.terminate()
        sleep(1)
        p_ms.terminate()
        p_alert.terminate()
        print('done')
        #clear_pipe()
    lastbtn=t

btn=Button(5)
btn.when_pressed=btn_pressed
btn.when_released=btn_released

try:
    while(True):
        sleep(1)

except KeyboardInterrupt:
    print("end")
    if p_connect is not None:
        p_connect.terminate()


        

