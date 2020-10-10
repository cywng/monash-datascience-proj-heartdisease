library(plumber)
r <- plumb("./API/plumber_api.R")
r$run(port = 8000)

# we can access it via *http://localhost:8000/predict*, but requires an input 
# in a JSON format. 

# Here is one we prepared earlier: 
#{"Typical.Chest.Pain":1,"Age":67,"Atypical":0,"FBS":80,"HTN":1,"DM":0,"EF.TTE":40,"K":4.7,"PR":80,"ESR":26,"TG":309,"Tinversion":1,"Lymph":38,"Neut":55,"St.Depression":1,"Dyspnea":0,"Nonanginal":0,"Region.RWMA2":0,"VHD.Mild":0,"PLT":165,"BMI":28.3987180007303,"Na":156}

#Paste the following line into r terminal (next to consle)
#curl -H "Content-Type: application/json" -X GET -d '{"Typical.Chest.Pain":1,"Age":67,"Atypical":0,"FBS":80,"HTN":1,"DM":0,"EF.TTE":40,"K":4.7,"PR":80,"ESR":26,"TG":309,"Tinversion":1,"Lymph":38,"Neut":55,"St.Depression":1,"Dyspnea":0,"Nonanginal":0,"Region.RWMA2":0,"VHD.Mild":0,"PLT":165,"BMI":28.3987180007303,"Na":156}' "http://localhost:8000/predict"