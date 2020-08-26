#!/usr/bin/python3
#-*- coding:utf-8 -*-

# Read values from sqlite3, and push them onto ZeroMQ, where they can be picked up and processed
# this should be faster than reading them in haskell, since python calls straight out to C libraries

import sqlite3
import sys
import logging
import zmq
import time #for sleep

try:
    loggingFileName='stream_sqlite_to_zmq_for_backtest.log'
    logging.basicConfig(format='%(asctime)s %(message)s',filename=loggingFileName,level=logging.INFO)
    logging.info("From dbFile={}, load secID={} from {} to {} and push onto port={}".format(sys.argv[1],sys.argv[2],sys.argv[3],sys.argv[4],sys.argv[5]))
    dbFileName=sys.argv[1]
    secID=sys.argv[2]
    startTS=sys.argv[3]
    endTS=sys.argv[4]
    portNum=sys.argv[5]

    #Set up client connection to price server
    context = zmq.Context()
    socket = context.socket(zmq.PAIR)
    socket.bind("tcp://127.0.0.1:{}".format(portNum))
    time.sleep(1)  #sleep for 2sec to allow zmq to set up the connection with the subscriber
    socket.send_string("start")  #send opening string, to ensure the receiver has somthing to subscribe to (it doesn't exist if no message published)
    logging.info("Sent start on port={}".format(portNum))

    #Read from SQL
    conn=sqlite3.connect(dbFileName,timeout=120)  # 2min timeout should cover it
    cursor = conn.cursor()
    # for row in cursor.execute('''SELECT timestamp, bid, offer, state FROM ticks'''):  #takes real ~1m34.6s
    # for row in cursor.execute('''SELECT timestamp, bid, offer, state FROM ticks WHERE symbol=?''', ('L1:CS.D.GBPUSD.CFD.IP')):
    for row in cursor.execute('''SELECT timestamp, bid, offer, state FROM ticks WHERE symbol=? AND timestamp>? AND timestamp<? ''', (secID,startTS,endTS)):
        # print("{} {} {} {}".format(int(row[0]),row[1],row[2],row[3]))
        socket.send_string("{} {} {} {}".format(int(row[0]),row[1],row[2],row[3]))  #publish on zmq
    socket.send_string("end")  #send termination string
    logging.info("Sent end on port={}".format(portNum))
    socket.close()
    context.term()
    logging.info("Completed port={}".format(portNum))

    logging.info("------------- Script done {} -------------".format(portNum))
except:
    logging.info("Unexpected error: ", sys.exc_info()[0])
    raise
