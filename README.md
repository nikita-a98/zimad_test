# Zimad Test Task

### Start application

- ```cd zimad_test```
- ```make run```

---


### Test

`zimad:register(<<"nikis">>).`  
`<<"{\"error\":\"already_exists\"}">>`  

`zimad:register(<<"nikitas">>).`  
`<<"\"13d24073-4f14-48da-ae89-518491acf3dc\"">>`  

`zimad:authorize(<<"13d24073-4f14-48da-ae89-518491acf3dc">>).`  
`<<"\"eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJ1aWQiOiIxM2QyNDA3My00ZjE0LTQ4ZGEtYWU4OS01MTg0OTFhY2YzZGMiLCJqdGkiOiI1YTU5NzU5MS1jYTk4LTRkOTYtYWIyYS1hMjY4OTVhOGIzNjMiLCJpYXQiOjE1NzUwNTEyNzMsImV4cCI6MTU3NTA1MjE3M30.xsysicnogTerekZJaYxMmFoQuzXxI5f7hlP7Z5Z4fkw\"">>.`  

`zimad:get_profile(<<"eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJ1aWQiOiIxM2QyNDA3My00ZjE0LTQ4ZGEtYWU4OS01MTg0OTFhY2YzZGMiLCJqdGkiOiI1YTU5NzU5MS1jYTk4LTRkOTYtYWIyYS1hMjY4OTVhOGIzNjMiLCJpYXQiOjE1NzUwNTEyNzMsImV4cCI6MTU3NTA1MjE3M30.xsysicnogTerekZJaYxMmFoQuzXxI5f7hlP7Z5Z4fkw">>).`  
`<<"{\"user_id\":\"13d24073-4f14-48da-ae89-518491acf3dc\",\"stars\":0,\"nickname\":\"nikitas\",\"level\":0,\"coins\":100}">>`  

`zimad:get_profile(<<"eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1TerekZJaYxMmFoQuzXxI5f7hlP7Z5Z4fk">>).`   
`<<"{\"error\":\"invalid_sign\"}">>`  

`zimad:get_profile(<<"eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJ1aWQiOiIxM2QyNDA3My00ZjE0LTQ4ZGEtYWU4OS01MTg0OTFhY2YzZGMiLCJqdGkiOiI1YTU5NzU5MS1jYTk4LTRkOTYtYWIyYS1hMjY4OTVhOGIzNjMiLCJpYXQiOjE1NzUwNTEyNzMsImV4cCI6MTU3NTA1MjE3M30.xsysicnogTerekZJaYxMmFoQuzXxI5f7hlP7Z5Z4fkw">>).`  
`<<"{\"error\":\"invalid_jwt\"}">>`  

`zimad:win_level(<<"eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJ1aWQiOiIxM2QyNDA3My00ZjE0LTQ4ZGEtYWU4OS01MTg0OTFhY2YzZGMiLCJqdGkiOiJkMjdiMTA2YS0wYjM0LTQwZjMtOGEyNC1jMWFmYTFjOTJhM2UiLCJpYXQiOjE1NzUwNTE0MDgsImV4cCI6MTU3NTA1MjMwOH0._0xNHwUk6vYilBd4Yh1oygdsRcT5BpVVtfiRe094m3g">>).`  
`<<"{\"status\":\"ok\"}">>`  

`zimad:win_level(<<"eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJ1aWQiOiIxM2QyNDA3My00ZjE0LTQ4ZGEtYWU4OS01MTg0OTFhY2YzZGMiLCJqdGkiOiJkMjdiMTA2YS0wYjM0LTQwZjMtOGEyNC1jMWFmYTFjOTJhM2UiLCJpYXQiOjE1NzUwNTE0MDgsImV4cCI6MTU3NTA1MjMwOH0._0xNHwUk6vYilBd4Yh1oygdsRcT5BpVVtfiRe094m3g">>).`  
`<<"{\"status\":\"ok\"}">>`  

`zimad:get_profile(<<"eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJ1aWQiOiIxM2QyNDA3My00ZjE0LTQ4ZGEtYWU4OS01MTg0OTFhY2YzZGMiLCJqdGkiOiJkMjdiMTA2YS0wYjM0LTQwZjMtOGEyNC1jMWFmYTFjOTJhM2UiLCJpYXQiOjE1NzUwNTE0MDgsImV4cCI6MTU3NTA1MjMwOH0._0xNHwUk6vYilBd4Yh1oygdsRcT5BpVVtfiRe094m3g">>).`  
`<<"{\"user_id\":\"13d24073-4f14-48da-ae89-518491acf3dc\",\"stars\":0,\"nickname\":\"nikitas\",\"level\":2,\"coins\":100}">>`  

`zimad:buy_stars(<<"eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJ1aWQiOiIxM2QyNDA3My00ZjE0LTQ4ZGEtYWU4OS01MTg0OTFhY2YzZGMiLCJqdGkiOiJkMjdiMTA2YS0wYjM0LTQwZjMtOGEyNC1jMWFmYTFjOTJhM2UiLCJpYXQiOjE1NzUwNTE0MDgsImV4cCI6MTU3NTA1MjMwOH0._0xNHwUk6vYilBd4Yh1oygdsRcT5BpVVtfiRe094m3g">>, 100).`  
`<<"{\"error\":\"not_enough_coin\"}">>`  

`zimad:buy_stars(<<"eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJ1aWQiOiIxM2QyNDA3My00ZjE0LTQ4ZGEtYWU4OS01MTg0OTFhY2YzZGMiLCJqdGkiOiJkMjdiMTA2YS0wYjM0LTQwZjMtOGEyNC1jMWFmYTFjOTJhM2UiLCJpYXQiOjE1NzUwNTE0MDgsImV4cCI6MTU3NTA1MjMwOH0._0xNHwUk6vYilBd4Yh1oygdsRcT5BpVVtfiRe094m3g">>, 5).`   
`<<"{\"status\":\"ok\",\"stars\":5}">>`  

`zimad:get_profile(<<"eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJ1aWQiOiIxM2QyNDA3My00ZjE0LTQ4ZGEtYWU4OS01MTg0OTFhY2YzZGMiLCJqdGkiOiJkMjdiMTA2YS0wYjM0LTQwZjMtOGEyNC1jMWFmYTFjOTJhM2UiLCJpYXQiOjE1NzUwNTE0MDgsImV4cCI6MTU3NTA1MjMwOH0._0xNHwUk6vYilBd4Yh1oygdsRcT5BpVVtfiRe094m3g">>).`     
`<<"{\"user_id\":\"13d24073-4f14-48da-ae89-518491acf3dc\",\"stars\":5,\"nickname\":\"nikitas\",\"level\":2,\"coins\":50}">>`  

`zimad:gdpr_erase_profile(<<"eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJ1aWQiOiIxM2QyNDA3My00ZjE0LTQ4ZGEtYWU4OS01MTg0OTFhY2YzZGMiLCJqdGkiOiJkMjdiMTA2YS0wYjM0LTQwZjMtOGEyNC1jMWFmYTFjOTJhM2UiLCJpYXQiOjE1NzUwNTE0MDgsImV4cCI6MTU3NTA1MjMwOH0._0xNHwUk6vYilBd4Yh1oygdsRcT5BpVVtfiRe094m3g">>).`   
`<<"{\"status\":\"ok\"}">>`  

`zimad:gdpr_erase_profile(<<"eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJ1aWQiOiIxM2QyNDA3My00ZjE0LTQ4ZGEtYWU4OS01MTg0OTFhY2YzZGMiLCJqdGkiOiJkMjdiMTA2YS0wYjM0LTQwZjMtOGEyNC1jMWFmYTFjOTJhM2UiLCJpYXQiOjE1NzUwNTE0MDgsImV4cCI6MTU3NTA1MjMwOH0._0xNHwUk6vYilBd4Yh1oygdsRcT5BpVVtfiRe094m3g">>).`  
`<<"{\"error\":\"invalid_jwt\"}">>`  

----