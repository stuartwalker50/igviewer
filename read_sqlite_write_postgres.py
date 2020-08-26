#!/usr/bin/python3
#-*- coding:utf-8 -*-

# Read values from sqlite3, and write them to postgresql

import sqlite3
import sys
import logging
import time #for sleep
import psycopg2

loggingFileName='read_sqlite_write_to_postgresql.log'
logging.basicConfig(format='%(asctime)s %(message)s',filename=loggingFileName,level=logging.INFO)
sqlite3Filename=sys.argv[1]
secID=sys.argv[2]
tableName="raw_ticks_"+secID[3:].replace('.','_')  #drop the leading 'L1:'
logging.info("Writing {} ticks from {} into table={} in postgres".format(secID,sqlite3Filename,tableName))

try:  #catch all errors and log them
    connPostgres = psycopg2.connect("dbname=backtest user=stuart password=stuart")  #connect to localhost on standard port 5432
    curPostgres  = connPostgres.cursor()
    try:  #create table if not already there
        queryStrCreate = "CREATE TABLE {} (".format(tableName) + """
                timestamp INT,
                bid REAL,
                offer REAL,
                mktstate TEXT
            )
            """
        logging.info("Query string is:")
        logging.info(queryStrCreate)
        curPostgres.execute(queryStrCreate)
    except: #create table if not already there
        print("Couldn't create table, does it already exist? Error: {} ".format(sys.exc_info()[0]))
        logging.info("Couldn't create table, does it already exist? Error: {} ".format(sys.exc_info()[0]))
    # close communication with the postgresql database server
    curPostgres.close()
    connPostgres.commit()
    # commit the changes

    #Now do the read/write
    curPostgres = connPostgres.cursor()
    connSqlite3 = sqlite3.connect(sqlite3Filename,timeout=12)  # short timeout should cover it
    curSqlite3  = connSqlite3.cursor()
    for row in curSqlite3.execute('''SELECT timestamp, bid, offer, state FROM ticks WHERE symbol=?''', (secID,)):
        # print("INSERT INTO {} (timestamp,bid,offer,mktstate) VALUES ({},{},{},\'{}\') ; ".format(tableName,row[0],row[1],row[2],row[3]))  # DEBUG only
        curPostgres.execute("INSERT INTO {} (timestamp,bid,offer,mktstate) VALUES ({},{},{},\'{}\') ; ".format(tableName,row[0],row[1],row[2],row[3]))

    curPostgres.close()
    connPostgres.commit()
    logging.info("Write complete {} {} {}".format(sqlite3Filename,secID,tableName))
except: #catch all errors and log them
    print("{} Unexpected error: ".format(sys.exc_info()[0]))
    logging.info("{} Unexpected error: ".format(sys.exc_info()[0]))
    raise

    # #Read from SQL
    # cursor = conn.cursor()
    # # for row in cursor.execute('''SELECT timestamp, bid, offer, state FROM ticks'''):  #takes real ~1m34.6s
    # # for row in cursor.execute('''SELECT timestamp, bid, offer, state FROM ticks WHERE symbol=?''', ('L1:CS.D.GBPUSD.CFD.IP')):
    # for row in cursor.execute('''SELECT timestamp, bid, offer, state FROM ticks WHERE symbol=? AND timestamp>? AND timestamp<? ''', (secID,startTS,endTS)):
    #     opFile.write("{} {} {} {} \n".format(int(row[0]),row[1],row[2],row[3]))  #write to file
    # opFile.write("end")  #write termination string to file, that the reading program is expecting to mark the end of the file
    # logging.info("----- Completed dbFile={}, load secID={} from {} to {} ref={}".format(sys.argv[1],sys.argv[2],sys.argv[3],sys.argv[4],sys.argv[5]))
