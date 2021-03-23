#include <Audio.h>
#include <Wire.h>
#include <SPI.h>
#include <SD.h>
#include <SerialFlash.h>

#include "AudioSampleTeensy.h"

const int PIN_INT=33;

AudioPlayMemory play1;
AudioPlayMemory play2;
AudioPlayMemory play3;
AudioPlayMemory play4;
AudioOutputTDM           tdm1;           //xy=819,547
AudioConnection          patchCord1(play1, 0, tdm1, 0);
AudioConnection          patchCord2(play2, 0, tdm1, 2);
AudioConnection          patchCord3(play3, 0, tdm1, 12);
AudioConnection          patchCord4(play4, 0, tdm1, 14);
AudioControlCS42448      cs42448_1;      //xy=998,551

elapsedMicros micro;
long unsigned int last_clk;
long unsigned int extclk_cnt;
 
int led = 13;
bool codecEnabled = false;
void setup() {
  // put your setup code here, to run once:
  Serial.begin(9600);
  AudioMemory(10);

  codecEnabled = cs42448_1.enable();
  if(codecEnabled) {
    digitalWrite(led, HIGH);
    cs42448_1.volume(0.6);
   }
   
  //layout:
  //3 4
  //2 1

  delay(1000);
  play1.play(AudioSampleTeensy, 0);
  play2.play(AudioSampleTeensy, 250);
  play3.play(AudioSampleTeensy, 500);
  play4.play(AudioSampleTeensy, 750);
  
  pinMode(PIN_INT, INPUT);
  micro=0;
  last_clk=0;
  extclk_cnt=0;
  attachInterrupt(PIN_INT, extclk, FALLING);

  delay(10);
}

void extclk(){
  extclk_cnt++;
  if(extclk_cnt==10){ // 50k clock = 1s
    uint32_t m=(uint32_t)micro;
    micro=0;
    last_clk=m;
    extclk_cnt=0;
  }
}

void loop() {
  // put your main code here, to run repeatedly:
  Serial.println(last_clk);
  delay(1000);
}
