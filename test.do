clear 
import excel "C:\Users\pf.RF\Dropbox (ROCKWOOL Foundation)\rff\Fallesen_2013 reform\log_incident.xlsx", sheet("Sheet13") firstrow
gen time = _n
tsset time

graph twoway (line log time) (scatter log time)

replace log = log(log)

tstf log , int(92) decay pathr("C:\Program Files\R\R-3.5.1\bin\x64\R.exe") ///
		arima(1 ,0 ,4) sarima(1,0 ,0 ,12)  greffect grdata tabulate

tstf log, /// 
		int(91) step pathr("C:\Program Files\R\R-3.5.1\bin\x64\R.exe") ///
		arima(1 ,0 ,1) sarima(1,0 ,0 ,12)  greffect grdata tabulate			
tstf log if time != 91, /// 
		 int(91) step pathr("C:\Program Files\R\R-3.5.1\bin\x64\R.exe") ///
		arima(1 ,0 ,1) sarima(1,0 ,0 ,12)  greffect grdata tabulate		
tstf log if time != 91 & time != 92, /// 
		 int(91) step pathr("C:\Program Files\R\R-3.5.1\bin\x64\R.exe") ///
		arima(1 ,0 ,1) sarima(1,0 ,0 ,12)  greffect grdata tabulate		
tstf log if time != 91 & time != 92 & time != 93, /// 
		 int(91) step pathr("C:\Program Files\R\R-3.5.1\bin\x64\R.exe") ///
		arima(1 ,0 ,1) sarima(1,0 ,0 ,12)  greffect grdata tabulate		
tstf log if time != 91 & time != 92 & time != 93 & time != 94, /// 
		 int(91) step pathr("C:\Program Files\R\R-3.5.1\bin\x64\R.exe") ///
		arima(1 ,0 ,1) sarima(1,0 ,0 ,12)  greffect grdata tabulate				
	
tstf log if time != 91 & time != 92 & time != 93 & time != 94, /// 
		 int(95) decay pathr("C:\Program Files\R\R-3.5.1\bin\x64\R.exe") ///
		arima(1 ,0 ,1) sarima(1,0 ,0 ,12)  greffect grdata tabulate				
