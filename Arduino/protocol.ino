#include <Adafruit_NeoPixel.h>
#include <avr/power.h>

#define PIN            13
#define NUMPIXELS      30

Adafruit_NeoPixel pixels = Adafruit_NeoPixel(NUMPIXELS, PIN, NEO_GRB + NEO_KHZ800);

void setup() {
  Serial.begin(115200);
  pixels.begin();
}

byte mode = 0;
short pixelsIncoming = 0;
void loop() {
  if (Serial.available() > 0) {
    switch mode {
      case 0: // SetMode
        mode = Serial.read();
        break;
      case 1: // SetPixels(pixelLength)
        if (Serial.available() > 1) {
          byte higher = Serial.read();
          pixelsIncoming = higher << 8 | Serial.read();
        }
        mode = 2;
        break;
      case 2: // SetPixels(pixelData)
        if (Serial.available() > 3) {
          uint8_t pixel = Serial.read();
          byte r = Serial.read();
          byte g = Serial.read();
          byte b = Serial.read();
          pixels.setPixelsColor(pixel, pixels.Color(r,g,b));
        }
        pixelsIncoming--;
        if (pixelsIncoming == 0) {
          mode = 0; 
        }
        break;
      case 3: // ShowPixels
        pixels.show();
        mode = 0;
        break;
      case 4: // RandomWalkThrough
        randomWalkThrough(random(3,10), random(1,50), random(5,10));
        mode = 0;
        break;
    }
  }
}
