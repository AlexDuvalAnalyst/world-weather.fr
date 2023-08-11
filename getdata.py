from meteostat import Daily, Hourly, Monthly, Stations, Normals
from datetime import datetime
import pandas

#datetime_str = '2022-09-19 00:00:00'
#datetime.strptime(datetime_str, '%Y-%m-%d %H:%M:%S')

def stations():
    st = Stations()
    station = st.fetch()
    station.to_csv('stations.csv',sep = ";")
    return 
    
def getdt(lieu,method,dt1,dt2):
    st = Stations()
    station = st.fetch()
    start = datetime.strptime(dt1, '%Y-%m-%d %H:%M:%S')
    end = datetime.strptime(dt2, '%Y-%m-%d %H:%M:%S')
    site = station[station.name == lieu].wmo.iloc[0]
    data = method(site, start, end)
    data = data.fetch()
    return data

