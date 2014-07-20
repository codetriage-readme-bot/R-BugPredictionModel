import csv
import array
from datetime import datetime,date
from dateutil.relativedelta import relativedelta

def getDate(date1):
    formatter_string = "%d-%m-%Y" 
    datetime_object1 = datetime.strptime(date1, formatter_string)
    return datetime_object1.date()

def getMonthRel(date1,month):
    date_after_month = date1+ relativedelta(months=month)
    return date_after_month

with open('2.csv', 'rb') as csvfile:
    data = csv.reader(csvfile, delimiter=',')
    # Getting first date delimiter=',')
    firstRow = 1
    lastRow = sum(1 for index in data)
    csvfile.seek(0)
    data.__init__(csvfile,delimiter=",")
    rowCount = 0
    date1 = ''
    date2 = ''
    month=3
    for row in data:
        rowCount = rowCount + 1
        if firstRow == rowCount:
            date1=row
            date1 = getDate(date1[0].split(" ")[0])
        elif lastRow == rowCount:
            date2 = row
            date2 = getDate(date2[0].split(" ")[0])
    csvfile.seek(0)
    data.__init__(csvfile,delimiter=",")
    dates = []
    date_end = date2
    date_start = date1
    while (date_start<date_end):
        date_start=getMonthRel(date_start,month)
        dates.append(date_start)
    date_end = date2
    date_start = date1
    for x in dates:
        i=0
        for row in data:
            date_object1 = date_start
            date_object2 = x
            date=row
            date = date[0].split(" ")[0]
            date_obj = getDate(date)
            if (date_obj >= date_object1 and date_obj < date_object2):
                 i = i + 1
        print i
        csvfile.seek(0)
        data.__init__(csvfile,delimiter=",")
        date_start=x









    