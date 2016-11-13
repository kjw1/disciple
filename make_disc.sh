#!/bin/bash
DISCIPLE=$(curl -sLv -X POST -H 'content-type: application/json' -d '{"name": "Bob Bobberson"}' localhost:8080/1/disciple)
curl -sLv -X GET localhost:8080/1/disciple
ID=$(echo $DISCIPLE | jq -r '.id' )
echo $ID
curl -sLv -X POST -H 'content-type: application/json' -d '{"feedback_type": "encourage"}' localhost:8080/1/disciple/$ID/feedback
AD=$(curl -sLv -X POST -H 'content-type: application/json' -d '{"name": "go go", "stages": []}' localhost:8080/1/adventure)

ADID=$(echo $AD | jq -r '.id' )
echo $ADID
curl -sLv -X GET -H 'content-type: application/json' localhost:8080/1/adventure/$ADID
curl -sLv -X POST -H 'content-type: application/json' -d '{"name": "go go go", "stages": []}' localhost:8080/1/adventure/$ADID
