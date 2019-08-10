import matplotlib
import matplotlib.pyplot as plt
import pandas as pd

data = pd.read_table('sample.c7.in',header=None,skiprows=59)
df = pd.DataFrame({
	'year':data[0],
	'doy':data[1],
	'local':data[2],
	'windspeed':data[3],
	'NA':data[4],
	'radflux':data[5],
	'NA':data[6],
	'NA':data[7],
	'NA':data[8],
	'airtemp':data[9],
	'airwatervapor':data[10],
	'precip':data[11],
	'preciptype':data[12],
	'idxx':data[13]
})
# plt.subplot(2,2,1)
df.plot(kind ='line', y='windspeed')

# plt.subplot(2,2,2)
df.plot(kind ='line', y='radflux')

# plt.subplot(2,2,3)
df.plot(kind ='line', y='airtemp')

# plt.subplot(2,2,4)
df.plot(kind ='line', y='airwatervapor')

plt.show()