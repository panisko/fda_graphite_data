--statusPiec = fibaro:getGlobal("Piec");

local device = fibaro:getSelfId();
local ipaddress = fibaro:getValue(device, "IPAddress");
local port = fibaro:getValue(device, "TCPPort");
local socket = Net.FTcpSocket(ipaddress, port); 
socket:setReadTimeout(1000);

Debug = function ( color, message )
  fibaro:debug(string.format('<%s style="color:%s;">%s', "span", color, message))
end

Send = function (deviceId , suffix, metric)
  	local ipaddress = fibaro:getValue(device, "IPAddress");
	local port = fibaro:getValue(device, "TCPPort");
	local socket = Net.FTcpSocket(ipaddress, port); 
	socket:setReadTimeout(250);
  	local name = fibaro:get(deviceId, 'userDescription');
  	--fibaro:debug(name);
  	local value = fibaro:getValue(deviceId, metric);
 	local msg = "home.fibaro." .. name .. "." ..suffix .. " " .. value.. " " .. os.time() .." \n"; 
  	--Debug("green", msg)
  	local bytes, errorCode = socket:write(msg); 
  	
end


-- temperatura
--tab={71, 56, 121, 48, 52, 75, 15, 79}
tab = fibaro:getDevicesId({type = "com.fibaro.temperatureSensor"})
--local = 
for k,v in ipairs(tab) 
do 
  
  	Send(v,"temp", "value")
  	
end

--tab = {51, 70, 55, 120, 47, 14, 74 }
tab = fibaro:getDevicesId({type = "com.fibaro.setPoint"})

for k,v in ipairs(tab) 
do 
  	Send(v,"targetTemp", "targetLevel")
  	Send(v,"mode", "mode")
  	Send(v,"valve", "value")
  	Debug("green", v)
end



--Status pieca
local value = fibaro:getValue(87, "ui.Label1.value")

if ( string.find(value, 'ERROR') ~= nil )
then
  	local msg = "home.fibaro.piec.status 99 " .. os.time() .." \n"; 
  	local bytes, errorCode = socket:write(msg);
end

if ( tonumber(string.find(value,"ON")) ~= nil  )
then
    local msg = "home.fibaro.piec.status 1 " .. os.time() .." \n"; 
  	--Debug("green", msg)
  	local bytes, errorCode = socket:write(msg);
end
if ( tonumber(string.find(value,"OFF")) ~= nil )
then	
   	local msg = "home.fibaro.piec.status 0 " .. os.time() .." \n"; 
  	--Debug("green", msg)
  	local bytes, errorCode = socket:write(msg);
end


-- send weather
local bytes, errorCode = socket:write("home.fibaro.pogoda.wilgotnosc " ..fibaro:getValue(3, "Humidity") .." " .. os.time() .." \n" );
local bytes, errorCode = socket:write("home.fibaro.pogoda.temp " ..fibaro:getValue(3, "Temperature") .." " .. os.time() .." \n" );
local bytes, errorCode = socket:write("home.fibaro.pogoda.wiatr " ..fibaro:getValue(3, "Wind") .." " .. os.time() .." \n" );
local bytes, errorCode = socket:write("home.fibaro.pogoda.cisnienie " ..fibaro:getValue(3, "Pressure") .." " .. os.time() .." \n" );

--send heating status
local virtualDeviceHeating = fibaro:getDevicesId({type = "virtual_device" , name ="Termostat"})[1]
local status = fibaro:getValue(virtualDeviceHeating, "ui.Label7.value")
if (string.find(status, 'Grzanie')  ~= nil)
then
        local msg = "home.fibaro.grzanie.status 1 " .. os.time() .." \n";
        local bytes, errorCode = socket:write(msg);
  		fibaro:debug("grzanie")
end
if (string.find(status, 'Wietrzenie')  ~= nil)
then
        local msg = "home.fibaro.grzanie.status 0 " .. os.time() .." \n";
        local bytes, errorCode = socket:write(msg);
  		fibaro:debug("wietrzenia")
end
local switchPointValve = fibaro:getGlobal("switchPointValve")
local msg = "home.fibaro.grzanie.switchPointValve ".. switchPointValve .. " " .. os.time() .." \n";
local bytes, errorCode = socket:write(msg);
fibaro:debug(msg)
