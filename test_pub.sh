#!/bin/bash

invalid_topic=iiot/
topic=iiot/company/public/test
col1=time
col2=iid
col3=metric
col4=value
# Invalid JSON Payload
echo "Publishing invalid JSON Payload...."
mosquitto_pub -m "{'$col1','test_time', '$col3' : 'test_metric', '$col4' : 'Invalid JSON Payload'}"  -t $topic

# Invalid topic length ,topic without schema and table
echo "Publishing invalid topic length ,topic without schema and table...."
mosquitto_pub -m "{\"$col1\":\"test_time\", \"$col3\" : \"test_metric\", \"$col4\" : \"Invalid topic length ,topic without schema and table\"}"  -t $invalid_topic

# Invalid time
echo "Publishing invalid time 1...."
mosquitto_pub -m "{\"$col1\":\"$(date '+%s')\",\"$col2\":\"test_id\", \"$col3\" : \"test_metric\", \"$col4\" : \"invalid time 1\"}"  -t $topic
echo "Publishing invalid time 2...."
mosquitto_pub -m "{\"$col1\":\"$(date '+%s')$(date '+%N')\",\"$col2\":\"test_id\", \"$col3\" : \"test_metric\", \"$col4\" : \"invalid time 2\"}"  -t $topic

# Correct message
echo "Publishing correct message...."
mosquitto_pub -m "{\"$col1\":\"$(date +'%Y-%m-%d %H:%M:%S.%N %Z')\",\"$col2\":\"test_id\", \"$col3\" : \"test_metric\", \"$col4\" : \"test_value\"}"  -t $topic
