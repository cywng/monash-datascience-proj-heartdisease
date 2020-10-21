library(plumber)
r <- plumb("./API/plumber_api.R")
r$run(port = 8000)

# we can access it via *http://localhost:8000/predict*, but requires an input 
# in a JSON format. 

# Here is one we prepared earlier: 

#Paste the following line into r terminal (next to consle)
#Positive case:
# curl -H "Content-Type: application/json" -X GET -d '{"Age":53,"DM":"0","HTN":"1","BP":110,"Typical.Chest.Pain":"0","Atypical":"N","FBS":90,"TG":250,"ESR":7,"K":4.7,"EF.TTE":50,"Region.RWMA":"0"}' "http://localhost:8000/predict"

#negative case:
# curl -H "Content-Type: application/json" -X GET -d '{"Age":58,"DM":"0","HTN":"0","BP":90,"Typical.Chest.Pain":"0","Atypical":"N","FBS":69,"TG":79,"ESR":5,"K":3.4,"EF.TTE":50,"Region.RWMA":"0"}' "http://localhost:8000/predict"