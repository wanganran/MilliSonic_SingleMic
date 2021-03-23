/* Audio Library for Teensy 3.X
 * Copyright (c) 2014, Paul Stoffregen, paul@pjrc.com
 *
 * Development of this audio library was funded by PJRC.COM, LLC by sales of
 * Teensy and Audio Adaptor boards.  Please support PJRC's efforts to develop
 * open source software by purchasing Teensy or other PJRC products.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice, development funding notice, and this permission
 * notice shall be included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */

#include <Arduino.h>
#include <string.h>
#include "play_memory.h"
#include "utility/dspinst.h"


void AudioPlayMemory::play(const unsigned int *data, const int offset)
{
	uint32_t format;

	playing = 0;
	prior = 0;
	format = *data++;
	next = data+offset;
	beginning = data;
	length = format & 0xFFFFFF;
	full_length = length;
	length-=offset*2; //only PCM 44100kHz
	playing = format >> 24;

	// reverse
	//for(unsigned int i=0;i<length;i++){
	//	data[i+1]=(((data[i+1]&65536)<<16) | (data[i+1]>>16));
	//}	
}

void AudioPlayMemory::stop(void)
{
	playing = 0;
}

extern "C" {
extern const int16_t ulaw_decode_table[256];
};

void AudioPlayMemory::update(void)
{
	audio_block_t *block;
	const unsigned int *in;
	int16_t *out;
	uint32_t tmp32, consumed;
	int16_t s0, s1, s2, s3, s4;
	int i;

	if (!playing) return;
	block = allocate();
	if (block == NULL) return;

	//Serial.write('.');

	out = block->data;
	in = next;
	s0 = prior;

	if(length>AUDIO_BLOCK_SAMPLES){
		memcpy(out, in, sizeof(int)*AUDIO_BLOCK_SAMPLES/2);
		in+=AUDIO_BLOCK_SAMPLES/2;
		out+=AUDIO_BLOCK_SAMPLES;
		prior = s0;
		next = in;
		length -= AUDIO_BLOCK_SAMPLES;
	} else {
		int left=AUDIO_BLOCK_SAMPLES-length;

		memcpy(out, in, sizeof(int)*length/2);
		memcpy(out+length, beginning, sizeof(int)*left/2);
		length=full_length-left;
		next=beginning+left/2;
		
	}
	transmit(block);
	release(block);
}


#define B2M_88200 (uint32_t)((double)4294967296000.0 / AUDIO_SAMPLE_RATE_EXACT / 2.0)
#define B2M_44100 (uint32_t)((double)4294967296000.0 / AUDIO_SAMPLE_RATE_EXACT) // 97352592
#define B2M_22050 (uint32_t)((double)4294967296000.0 / AUDIO_SAMPLE_RATE_EXACT * 2.0)
#define B2M_11025 (uint32_t)((double)4294967296000.0 / AUDIO_SAMPLE_RATE_EXACT * 4.0)


uint32_t AudioPlayMemory::positionMillis(void)
{
	uint8_t p;
	const uint8_t *n, *b;
	uint32_t b2m;

	__disable_irq();
	p = playing;
	n = (const uint8_t *)next;
	b = (const uint8_t *)beginning;
	__enable_irq();
	switch (p) {
	  case 0x81: // 16 bit PCM, 44100 Hz
		b2m = B2M_88200;  break;
	  case 0x01: // u-law encoded, 44100 Hz
	  case 0x82: // 16 bits PCM, 22050 Hz
		b2m = B2M_44100;  break;
	  case 0x02: // u-law encoded, 22050 Hz
	  case 0x83: // 16 bit PCM, 11025 Hz
		b2m = B2M_22050;  break;
	  case 0x03: // u-law encoded, 11025 Hz
		b2m = B2M_11025;  break;
	  default:
		return 0;
	}
	if (p == 0) return 0;
	return ((uint64_t)(n - b) * b2m) >> 32;
}

uint32_t AudioPlayMemory::lengthMillis(void)
{
	uint8_t p;
	const uint32_t *b;
	uint32_t b2m;

	__disable_irq();
	p = playing;
	b = (const uint32_t *)beginning;
	__enable_irq();
	switch (p) {
	  case 0x81: // 16 bit PCM, 44100 Hz
	  case 0x01: // u-law encoded, 44100 Hz
		b2m = B2M_44100;  break;
	  case 0x82: // 16 bits PCM, 22050 Hz
	  case 0x02: // u-law encoded, 22050 Hz
		b2m = B2M_22050;  break;
	  case 0x83: // 16 bit PCM, 11025 Hz
	  case 0x03: // u-law encoded, 11025 Hz
		b2m = B2M_11025;  break;
	  default:
		return 0;
	}
	return ((uint64_t)(*(b - 1) & 0xFFFFFF) * b2m) >> 32;
}


