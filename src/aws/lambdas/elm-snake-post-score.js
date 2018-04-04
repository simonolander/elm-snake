var AWS = require('aws-sdk');
var dynamodb = new AWS.DynamoDB({apiVersion: '2012-08-10'});

exports.handler = (event, context, callback) => {
    var username = event.username;
    if (!username) {
        callback("invalid username: " + event.username, null);
        return;
    }
    
    var score = parseInt(event.score);
    if (isNaN(score) || score < 0) {
        callback("invalid score: " + event.score, null);
        return;
    }
    
    var tableName = "Scoreboard";
    var datetime = new Date().getTime().toString();
    var item = {
        "Game": {"S": "elm-snake"},
        "Score": {"N": String(score)},
        "Name": {"S": username},
        "DateTime": {"S": datetime}
    };
    var params = {
        "Item": item,
        "TableName": tableName
    };
    
    dynamodb.putItem(params, function (err, data) {
        if (err) {
            callback(err, null)
        }
        else {
            var getParams = {
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
            });
        }
    });
};
