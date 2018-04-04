var AWS = require('aws-sdk');
var dynamodb = new AWS.DynamoDB({apiVersion: '2012-08-10'});

exports.handler = (event, context, callback) => {
    var params = {
        TableName: "Scoreboard",
        Limit: 10,
        KeyConditionExpression: "Game = :game",
        ExpressionAttributeValues: {
          ":game": { "S": "elm-snake" }
        },
        ScanIndexForward: false
    };
    
    dynamodb.query(params, function (err, data) {
        if (err) {
            console.error("Unable to query. Error:", JSON.stringify(err, null, 2));
            callback(JSON.stringify(err), null);
        }
        else {
            var scores = data.Items.map(function (item) {
                return {
                    name: item.Name.S,
                    score: parseInt(item.Score.N)
                };
            });
            callback(null, scores);
        }
    })
};
