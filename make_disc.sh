#!/bin/bash
curl -sLv -X POST -H 'content-type: application/json' -d '{"name": "Bob Bobberson"}' localhost:8080/1/disciple
curl -sLv -X GET localhost:8080/1/disciple
