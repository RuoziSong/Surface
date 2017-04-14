#!/usr/bin/python
import csv
import requests
import time
import json

appid = "35348ee9"
appkey = "874bc0a8aaafe29bbe84abaeb78fd57d"
with open('restaurant.csv', 'rb') as readfile, open('restaurant1.csv', 'a') as writefile:
	writer = csv.writer(writefile, delimiter=',')
	reader = csv.reader(readfile, delimiter=',')
	last_address = ""
	last_coord = (0, 0)

	for row in reader:
		if row[0] == "CAMIS":
			writer.writerow(row + ['lat', 'ing'])
			continue

		address = row[3] + row[4] + row[2]

		if address == last_address:
			writer.writerow(row + [last_coord[0], last_coord[1]])
			continue

		last_address = address
			
		url = "https://api.cityofnewyork.us/geoclient/v1/address.json?houseNumber={0}&street={1}&borough={2}&app_id={3}&app_key={4}".format(row[3], row[4], row[2], appid, appkey)

		while True:
			try:
				r = requests.get(url).json()
				lat = r['address']['latitude']
				lon = r['address']['longitude']
				writer.writerow(row + [lat, lon]) 
				print (lat, lon)
				break
			except Exception as e:
				print e
				print address
				writer.writerow(row + ['0.0', '0.0'])
				break

#https://api.cityofnewyork.us/geoclient/v1/address.json?houseNumber=30&street=ROCKFEELLER PLAZA&borough=Manhattan&app_id=35348ee9&app_key=874bc0a8aaafe29bbe84abaeb78fd57d
