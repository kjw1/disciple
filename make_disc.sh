#!/bin/bash
DISCIPLE=$(curl -sLv -X POST -H 'content-type: application/json' -d '{"name": "Bob Bobberson"}' localhost:8080/1/disciple)
curl -sLv -X GET localhost:8080/1/disciple
ID=$(echo $DISCIPLE | jq -r '.id' )
echo $ID
curl -sLv -X POST -H 'content-type: application/json' -d '{"feedback_type": "encourage"}' localhost:8080/1/disciple/$ID/feedback



STAGE=$(curl -sLv -X POST -H 'content-type: application/json' \
  -d '{
  "description": "eat a lollipop",
  "difficulty": 10,
  "success": {
    "message": "Ate it",
    "consequences": [
      {
        "stat":"health",
        "change": 1
      }
    ]
  },
  "failure": {
    "message": "Choked",
    "consequences": [
      {
        "stat":"health",
        "change": -1
      }
    ]
  }
}' localhost:8080/1/stage)

STAGEID=$(echo $STAGE | jq -r '.id' )
curl -sLv -X GET -H 'content-type: application/json' localhost:8080/1/stage/$STAGEID


AD=$(curl -sLv -X POST -H 'content-type: application/json' -d '{"name": "go go", "stages": []}' localhost:8080/1/adventure)

ADID=$(echo $AD | jq -r '.id' )
echo $ADID
curl -sLv -X GET -H 'content-type: application/json' localhost:8080/1/adventure/$ADID

cat <<EOF |
{
  "name": "go go go",
  "stages": [ "$STAGEID" ]
}
EOF
curl -sLv -X POST -H 'content-type: application/json' -d @- localhost:8080/1/adventure/$ADID
