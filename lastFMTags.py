
import requests
import json

lastfmurl = "http://ws.audioscrobbler.com/2.0/?method="
param = "artist.getTags"
mykey = ""


def ArtistFormat(artistString):
    list = artistString.split(" ")
    if len(list) > 1:
        return "%20".join(list)
    else:
        return artistString


def query_site(url, params, uid="", fmt="json"):
    params["fmt"] = fmt
    r = requests.get(url + uid, params=params)
    print "requesting", r.url

    if r.status_code == requests.codes.ok:
        return r.json()
    else:
        r.raise_for_status()


def pretty_print(data, indent=4):
    if type(data) == dict:
        print json.dumps(data, indent=indent, sort_keys=True)
    else:
        print data


# r = requests.get(
#     "http://ws.audioscrobbler.com/2.0/?method=artist.getTags&artist=Bent%20Knee&user=Riuchando&api_key=6b5cf89e88a239a6bf0db17918a327bb&format=json")
# print r.json()

if r.status_code == requests.codes.ok:
    pretty_print(r.json())
else:
    r.raise_for_status()

get = [lastfmurl+param,
       "artist="+ArtistFormat("Bent Knee"),
       "user=Riuchando",
       "api_key="+mykey,
       "format=json"]

print "&".join(get)
r = requests.get("&".join(get))

if r.status_code == requests.codes.ok:
    pretty_print(r.json())
else:
    r.raise_for_status()
