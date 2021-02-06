
--Funkcje
Debug = function ( color, message )
  fibaro:debug(string.format('<%s style="color:%s;">%s', "span", color, message))
end

ChangePiecStatus = function (piecId, newStatus) 
  local status = fibaro:getValue(piecId, "ui.Label1.value")
  Debug("white", "Current status: " ..status);
  if (status ~= newStatus)
    then
    	if (newStatus == "ON")
      		then
      		Debug("orange","Piec ON")
      		fibaro:call(piecId, "pressButton", 2)
      	end
    	if (newStatus == "OFF")
      		then
      		Debug("orange","Piec OFF")
      		fibaro:call(piecId, "pressButton", 3)
      	end
    
    fibaro:debug("Changing piec status. New status: " .. newStatus);
    end
end


HandlePiec = function(PiecVdId, TempVsTargetTemp)
    if (TempVsTargetTemp <= 1)
		then
      	Debug("green", "Wylaczam piec");
    	ChangePiecStatus(PiecVdId, "OFF")
	end

	if (TempVsTargetTemp > 1)
		then
      	Debug("green", "Wlaczam piec");
    	ChangePiecStatus(PiecVdId, "ON")
	end
end

--Variables
local TermostatVdId = 89
local TempVsTargetTemp = tonumber(fibaro:getValue( TermostatVdId , "ui.Label9.value" ))
local Status = fibaro:getValue( TermostatVdId , "ui.Label7.value" )

local PiecVdId = 87
local PiecStatus = fibaro:getValue(87, "ui.Label1.value")


--Main--
if (Status == "Grzanie")
	then
  	Debug("white", "Grzanie is ON")
	HandlePiec(PiecVdId, TempVsTargetTemp)
end

if (Status == "Wietrzenie")
	then
	Debug("green", "Wietrzenie włączone wyłaczam piec");
	ChangePiecStatus(PiecVdId, "OFF")
end


