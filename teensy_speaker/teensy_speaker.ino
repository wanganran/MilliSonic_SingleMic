#include <Audio.h>
#include <Wire.h>
#include <SPI.h>
#include <SD.h>
#include <SerialFlash.h>

#include "chirp.h"

const int PIN_INT=33;

AudioPlayMemory play1;
AudioPlayMemory play2;
AudioPlayMemory play3;
AudioPlayMemory play4;
AudioOutputI2SQuad       i2s_quad;      //xy=365,94
AudioConnection          patchCord1(play1, 0, i2s_quad, 2);
AudioConnection          patchCord2(play2, 0, i2s_quad, 3);
AudioConnection          patchCord3(play3, 0, i2s_quad, 0);
AudioConnection          patchCord4(play4, 0, i2s_quad, 1);

elapsedMicros micro;
long unsigned int last_clk;
long unsigned int extclk_cnt;
 
void setup() {
  // put your setup code here, to run once:
  Serial.begin(9600);
  AudioMemory(10);

  //layout:
  //2 3
  //1 4

  delay(1000);
  play2.play(AudioSampleTeensy, 0);
  play3.play(AudioSampleTeensy, 250);
  play1.play(AudioSampleTeensy, 500);
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
