#include "application.h"
#include "LiquidCrystal/LiquidCrystal.h"

LiquidCrystal lcd(D0, D1, D2, D3, D4, D5);

String displayedMessage = "Waiting for command";
int buttonState = 0;

void setup() {
  lcd.begin(16,2);
  lcd.print("Booting...");
  
  pinMode(A4, INPUT);

  Spark.function("setMessage",setMessage);
}

int setMessage(String message) {
   displayedMessage = message;
   
   return 0;
}

void loop() {
  lcd.setCursor(0, 0);
  
  buttonState = digitalRead(A4);
  if (buttonState == HIGH) {
    lcd.print("DOWN            ");
  } else {
    lcd.print("UP              ");
  }

  lcd.setCursor(0, 1);
  lcd.print(displayedMessage);
}
