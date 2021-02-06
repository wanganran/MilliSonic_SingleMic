#include <Audio.h>
#include <Wire.h>
#include <SPI.h>
#include <SD.h>
#include <SerialFlash.h>

#include "chirp.h"

AudioPlayMemory play1;
AudioPlayMemory play2;
AudioPlayMemory play3;
AudioPlayMemory play4;
AudioOutputI2SQuad       i2s_quad;      //xy=365,94
AudioConnection          patchCord1(play1, 0, i2s_quad, 2);
AudioConnection          patchCord2(play2, 0, i2s_quad, 3);
AudioConnection          patchCord3(play3, 0, i2s_quad, 0);
AudioConnection          patchCord4(play4, 0, i2s_quad, 1);

void setup() {
  // put your setup code here, to run once:
  Serial.begin(9600);
  AudioMemory(10);


  delay(1000);
  
  play1.play(AudioSampleTeensy, 0);
  delay(1000);
  
  play2.play(AudioSampleTeensy, 410);
  
  delay(1000);
  play3.play(AudioSampleTeensy, 819);
  
  delay(1000);
  play4.play(AudioSampleTeensy, 1229);
}

void loop() {
  // put your main code here, to run repeatedly:
  Serial.print(AUDIO_BLOCK_SAMPLES);
  delay(1000);
}
