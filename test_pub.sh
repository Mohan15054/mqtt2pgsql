#!/bin/bash

# Invalid JSON Payload
echo "Publishing invalid JSON Payload...."
mosquitto_pub -m "{'time','test_time', 'metric' : 'test_metric', 'value' : 'Invalid JSON Payload'}"  -t "iiot/company/public/test"

# Invalid topic length ,topic without schema and table
echo "Publishing invalid topic length ,topic without schema and table...."
mosquitto_pub -m "{\"time\":\"test_time\", \"metric\" : \"test_metric\", \"value\" : \"Invalid topic length ,topic without schema and table\"}"  -t "iiot"

# Invalid data
echo "Publishing invalid data 1...."
mosquitto_pub -m "{\"time\":\"$(date '+%s')\",\"iid\":\"test_id\", \"metric\" : \"test_metric\", \"value\" : \"invalid_data_1\"}"  -t "iiot/company/public/test/metrics"
echo "Publishing invalid data 2...."
mosquitto_pub -m "{\"time\":\"$(date '+%s')$(date '+%N')\",\"iid\":\"test_id\", \"metric\" : \"test_metric\", \"value\" : \"invalid_data_2\"}"  -t "iiot/company/public/test/metrics"

# Correct message
echo "Publishing correct message...."
mosquitto_pub -m "{\"time\":\"$(date +'%Y-%m-%d %H:%M:%S.%N %Z')\",\"iid\":\"test_id\", \"metric\" : \"test_metric\", \"value\" : \"test_value\"}"  -t "iiot/company/public/test/metrics"
