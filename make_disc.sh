#!/bin/bash
DISCIPLE=$(curl -sLv -X POST -H 'content-type: application/json' -d '{"name": "Bob Bobberson"}' localhost:8080/1/disciple)
curl -sLv -X GET localhost:8080/1/disciple
ID=$(echo $DISCIPLE | jq -r '.id' )
echo $ID
curl -sLv -X POST -H 'content-type: application/json' -d '{"feedback_type": "encourage"}' localhost:8080/1/disciple/$ID/feedback
curl -sLv -X POST -H 'content-type: application/json' -d '{"name": "go go", "stages": []}' localhost:8080/1/adventure
