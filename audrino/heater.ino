#include <ESP8266WiFi.h>
 
const char* ssid = ""; // fill in here your router or wifi SSID
const char* password = ""; // fill in here your router or wifi password

int value = LOW;

#define RELAY 13 // relay connected to  GPIO0
WiFiServer server(80);
 
void setup() 
{
  
  Serial.begin(115200); // must be same baudrate with the Serial Monitor
  Serial.println("Stopping AP");
  WiFi.softAPdisconnect(true);
  
 
  pinMode(RELAY,OUTPUT);
  digitalWrite(RELAY, HIGH);
 
  // Connect to WiFi network
  Serial.println();
  Serial.println();
  Serial.print("Connecting to ");
  Serial.println(ssid);
 
  WiFi.begin(ssid, password);
 
  while (WiFi.status() != WL_CONNECTED) 
  {
    delay(500);
    Serial.print(".");
  }
  Serial.println("");
  Serial.println("WiFi connected");
 
  // Start the server
  server.begin();
  Serial.println("Server started");
 
  // Print the IP address
  Serial.print("Use this URL to connect: http://");
  
  Serial.print(WiFi.localIP());
  Serial.println("/");
  
 
}
 
void loop() 
{
  // Check if a client has connected
  WiFiClient client = server.available();
  client.setTimeout(2000);
  if (!client) 
  {
    //Serial.print("#");
    return;
  }
 
  // Wait until the client sends some data
  Serial.println("new client");
  int retries = 0; 
  while(!client.available()&& (retries < 2002))
  {
    retries++;
    //Serial.print("$");
    delay(1);
  }
  if (retries > 2000){
    client.stop();
    return;
    }
  

  // Read the first line of the request
  String request = client.readStringUntil('\r');
  Serial.println(request);
  client.flush();
 
  // Match the request
    
  // Return the response
  client.println("HTTP/1.1 200 OK");
  client.println("Content-Type: text/html");
  client.println(""); //  this is a must
  client.println("<!DOCTYPE HTML>");
  client.println("<html>");
  client.println("<body>");
  if (request.indexOf("/RELAY=ON") != -1)  
  {
    Serial.println("RELAY=ON");
    digitalWrite(RELAY,LOW);
    client.println("ON");
  }
  if (request.indexOf("/RELAY=OFF") != -1)  
  {
    Serial.println("RELAY=OFF");
    digitalWrite(RELAY,HIGH);
    client.println("OFF");
  }
  if(request.indexOf("/RELAY=ON") == -1 and request.indexOf("/RELAY=OFF") == -1)
  {
    int status = digitalRead(RELAY);
    Serial.println(status);
    if (status == 0 ){
      client.println("ON");
    } else {
      client.println("OFF");}
    } 
  client.println("</body>");
  client.println("</html>");
 
  delay(1);
  Serial.println("Client disonnected");
  Serial.println("");
}
