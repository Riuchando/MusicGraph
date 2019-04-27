import requests
import json

wsurl = "http://ws.audioscrobbler.com/2.0/?method="
lastfmurl = "http://www.last.fm"
method = "auth.gettoken"

mykey = ""


def authgetTags(api_key):
    get = [wsurl+method,
           "api_key="+api_key,
           "format=json"]
    token = requests.get("&".join(get))
    return token.json()


def pretty_print(data, indent=4):
    if type(data) == dict:
        print json.dumps(data, indent=indent, sort_keys=True)
    else:
        print data


def authUser(api_key, token):
    get = [lastfmurl+"/api/auth/?" +
           "api_key="+api_key,
           "token="+token,
           "format=json"]
    authorization = requests.get("&".join(get))
    return authorization.json()


token = authgetTags(mykey)
pretty_print(token)
author = authUser(mykey, token["token"])
pretty_print(author)
