import requests
import time

venues = ['ICSE', 'ESEC%2FSIGSOFT_FSE', 'ASE', 'IEEE_Trans._Software_Eng.', 'ACM_Trans._Softw._Eng._Methodol.']
keywords = ['online defect predict', 'online fault predict', 'online bug predict','real world defect predict',
            'realworld fault predict', 'real world bug predict', 'defect case study predict','bug case study predict',
            'fault case study predict'] 

h = '1000'
for venue in venues:
    for keyword in keywords:
        url = 'https://dblp.org/search/publ/api?q=' + keyword +' venue:'+ venue + ':'
        
        r = requests.get(url, allow_redirects=True)
        # open(venue + keyword + '.xml', 'wb').write(r.content)
        with open("all.xml", "ab") as myfile:
            myfile.write(r.content)
        time.sleep(3)
