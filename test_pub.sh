#!/bin/bash


# Default values
default_topic="iiot/company/public/test"
default_col1="time"
default_invalid_topic="iiot/"
default_col2="iid"
default_col3="metric"
default_col4="value"

# Function to display help menu
display_help() {
    echo "Usage: $0 [--topic TOPIC] [--col1 COL1] [--col2 COL2] [--col3 COL3] [--col4 COL4] [--invalid-topic INVALID_TOPIC] [--help]"
    echo ""
    echo "Options:"
    echo "  --topic TOPIC           Specify the topic to publish messages to (default: $default_topic)"
    echo "  --col1 COL1             Specify the value for col1 (default: $default_col1)"
    echo "  --col2 COL2             Specify the value for col2 (default: $default_col2)"
    echo "  --col3 COL3             Specify the value for col3 (default: $default_col3)"
    echo "  --col4 COL4             Specify the value for col4 (default: $default_col4)"
    echo "  --invalid-topic INVALID_TOPIC Specify the invalid topic (default: $default_invalid_topic)"
    echo "  --help                  Display this help menu"
    exit 0
}

# Parse command-line arguments
while [[ $# -gt 0 ]]; do
    key="$1"
    case $key in
        --topic)
        topic="$2"
        shift # past argument
        shift # past value
        ;;
        --col1)
        col1="$2"
        shift # past argument
        shift # past value
        ;;
        --col2)
        col2="$2"
        shift # past argument
        shift # past value
        ;;
        --col3)
        col3="$2"
        shift # past argument
        shift # past value
        ;;
        --col4)
        col4="$2"
        shift # past argument
        shift # past value
        ;;
        --invalid-topic)
        invalid_topic="$2"
        shift # past argument
        shift # past value
        ;;
        --help)
        display_help
        ;;
        *)    # unknown option
        shift # past argument
        ;;
    esac
done

# If any parameter is not provided, use default values
topic="${topic:-$default_topic}"
col1="${col1:-$default_col1}"
col2="${col2:-$default_col2}"
col3="${col3:-$default_col3}"
col4="${col4:-$default_col4}"
invalid_topic="${invalid_topic:-$default_invalid_topic}"

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
