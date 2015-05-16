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
  switch (mode) {
    case 1: // SetPixels(pixelLength)
      if (Serial.available() > 1) {
        pixelsIncoming = Serial.read() << 8;
        pixelsIncoming = pixelsIncoming | Serial.read();
        mode = 2;
      }
      break;
    case 2: // SetPixels(pixelData)
      if (Serial.available() > 4) {
        uint16_t pixel = Serial.read() << 8;
        pixel = pixel | Serial.read();
        byte r = Serial.read();
        byte g = Serial.read();
        byte b = Serial.read();
        pixels.setPixelColor(pixel, pixels.Color(r,g,b));
        pixelsIncoming--;
        if (pixelsIncoming == 0) {
          mode = 0;
          Serial.print(1);
        }
      }
      break;
    case 3: // ShowPixels
      pixels.show();
      mode = 0;
      break;
    case 99:
      Serial.print(1);
      mode = 0;
      break;
    default: // SetMode
      if (Serial.available() > 0) {
        mode = Serial.read();
      }
      break;
  }
}
