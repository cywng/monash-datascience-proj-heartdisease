library(plumber)
r <- plumb("./API/plumber_api.R")
r$run(port = 8000)

# we can access it via *http://localhost:8000/predict*, but requires an input 
# in a JSON format. 

# Here is one we prepared earlier: 
#positive:
  #{"Typical.Chest.Pain":1,"Age":67,"Atypical":0,"FBS":80,"HTN":1,"DM":0,"EF.TTE":40,"K":4.7,"PR":80,"ESR":26,"TG":309,"Tinversion":1,"Lymph":38,"Neut":55,"St.Depression":1,"Dyspnea":0,"Nonanginal":0,"Region.RWMA2":0,"VHD.Mild":0,"PLT":165,"BMI":28.3987180007303,"Na":156}
#negative
  #{"Typical.Chest.Pain":0,"Age":53,"Atypical":0,"FBS":84,"HTN":0,"DM":0,"EF.TTE":55,"K":3.9,"PR":100,"ESR":17,"TG":75,"Tinversion":0,"Lymph":31,"Neut":60,"St.Depression":0,"Dyspnea":1,"Nonanginal":0,"Region.RWMA2":0,"VHD.Mild":0,"PLT":261,"BMI":25.7210322145387,"Na":140}


#Paste the following line into r terminal (next to consle)
#Positive case:
  #curl -H "Content-Type: application/json" -X GET -d '{"Age":53,"DM":"0","HTN":"1","BP":110,"Typical.Chest.Pain":"0","Atypical":"N","FBS":90,"TG":250,"ESR":7,"K":4.7,"EF.TTE":50,"Region.RWMA":"0"}' "http://localhost:8000/predict"
#negative case:
  #curl -H "Content-Type: application/json" -X GET -d '{"Age":58,"DM":"0","HTN":"0","BP":90,"Typical.Chest.Pain":"0","Atypical":"N","FBS":69,"TG":79,"ESR":5,"K":3.4,"EF.TTE":50,"Region.RWMA":"0"}' "http://localhost:8000/predict"