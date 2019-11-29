# Documentation API
  
## Table of Contents

* [Start application](#application) 
  * [Start application](#start)
  * [Stop application](#stop)
* [Users](#users) 
  * [User registration](#register)
  * [User authorization](#authorize)
  * [Get user profile](#get_profile)
  * [Del user profile](#del_profile)
* [Starts](#stars) 
  * [Buy stars](#buy_stars)
* [Level](#level) 
  * [Win level](#win_level)
  
---


## <a name="start">Application</a>

1. `сd zimad_test`
2. `make`

### <a name="start">Start application</a>

- `_rel/zimad_test_release/bin/zimad_test_release start`

### <a name="stop">Stop application</a>

- `_rel/zimad_test_release/bin/zimad_test_release stop`

---


## <a name="users">Users</a>

### <a name="register">User registration</a>

Request:
```json
{
  "obj": "users", 
  "op": "register"
}
```

Response (success):
```json
{
  "status": "ok",
  "data": {
    "uid": "UserIdentification"
  }
}
```

Response (error):
```json
{
  "status": "error",
  "error_id": "already_exists",
  "message": "Conflict"
}
```

browser (GET):
```
http://localhost:8080/user/register?nickname=Nickname
```

curl (GET):
```
curl -X GET http://localhost:8080/user/register?nickname=Nickname
```

curl (POST):
```
curl -X POST http://localhost:8080/user/register -H "Content-Type: application/json" -d "{\"nickname\":\"Nickname\"}"
```
---

### <a name="authorize">User authorize</a>

Request:
```json
{
  "obj": "users", 
  "op": "authorize"
}
```

Response (success):
```json
{
  "status": "ok",
  "data": {
    "token": "UserToken"
  }
}
```

Response (error):
```json
{
  "status": "error",
  "error_id": "not_found",
  "message": "Not Found"
}
```

browser (GET):
```
http://localhost:8080/user/authorize?uid=UserId
```

curl (GET):
```
curl -X GET http://localhost:8080/user/authorize?uid=UserId
```

curl (POST):
```
curl -X POST http://localhost:8080/user/authorize -H "Content-Type: application/json" -d "{\"uid\":\"UserId\"}"
```
---


### <a name="get_profile">Get user profile</a>

Request:
```json
{
  "obj": "user",
  "op": "get_profile"	
}
```

Response (success):
```json
{
  "status": "ok",
  "data": {
    "user_id": "f0acb472-ab94-4ba3-b282-c730e2f79a9b",
    "stars": 0,
    "nickname": "Nikita",
    "level": 1,
    "coins": 100
  } 	
}
```

browser (GET):
```
http://localhost:8080/user/get_profile?uid=UserId&key=Token
```

curl (GET):
```
curl -X GET http://localhost:8080/user/get_profile?uid=UserId&key=Token
```

curl (POST):
```
curl -X POST http://localhost:8080/user/get_profile?key=Token -H "Content-Type: application/json" -d "{\"uid\":\"UserId\"}"
```
---

### <a name="del_profile">Delete user profile</a>

Request:
```json
{
  "obj": "user",
  "op": "del_profile"	
}
```

Response (success):
```json
{
  "status": "ok"
}
```

browser (GET):
```
http://localhost:8080/user/del_profile?uid=UserId&key=Token
```

curl (GET):
```
curl -X GET http://localhost:8080/user/del_profile?uid=UserId&key=Token
```

curl (POST):
```
curl -X POST http://localhost:8080/user/del_profile?key=Token -H "Content-Type: application/json" -d "{\"uid\":\"UserId\"}"
```
---



## <a name="stars">Stars</a>

### <a name="buy_stars">Buy stars</a>

Request:
```json
{
  "obj": "stars",
  "op": "buy",
  "data": {
    "stars_count": 3
  }
}
```

Response (success):
```json
{
  "status": "ok"	
}
```

Response (not enough coin):
```json
{
  "status": "error",
  "error_id": "not_enough_coin",
  "message": "Not Enough Coin"
}
```

Response (not found):
```json
{
  "status": "error",
  "error_id": "not_found",
  "message": "Not Found"
}
```

browser (GET):
```
http://localhost:8080/stars/buy?stars_count=Count&key=Token
```
curl (GET):
```
curl -X GET http://localhost:8080/stars/buy?stars_count=Count&key=Token
```

curl (POST):
```
curl -X POST http://localhost:8080/stars/buy?key=Token -H "Content-Type: application/json" -d "{\"stars_count\":\"Count\"}"
```
---



## <a name="level">Level</a>

### <a name="win_level">Win level</a>

Request:
```json
{
  "obj": "stars",
  "op": "buy",
  "data": {
    "stars_count": 3
  }
}
```

Response (success):
```json
{
  "status": "ok"	
}
```

Response (not found):
```json
{
  "status": "error",
  "error_id": "not_found",
  "message": "Not Found"
}
```

browser (GET):
```
http://localhost:8080/level/win?key=Token
```
curl (GET):
```
curl -X GET http://localhost:8080/level/win?key=Token
```

curl (POST):
```
curl -X POST http://localhost:8080/level/win?key=Token -H "Content-Type: application/json"
```
---