# To experiment with this code freely you will have to run this code locally.
# We have provided an example json output here for you to look at,
# but you will not be able to run any queries through our UI.
import json
import requests
import csv
import time
import unicodecsv

BASE_URL = "http://musicbrainz.org/ws/2/"
ARTIST_URL = BASE_URL + "artist/"

query_type = {"simple": {},
              "atr": {"inc": "aliases+tags+ratings"},
              "aliases": {"inc": "aliases"},
              "releases": {"inc": "releases"}}


def query_site(url, params, uid="", fmt="json"):
    params["fmt"] = fmt
    r = requests.get(url + uid, params=params)
    print "requesting", r.url

    if r.status_code == requests.codes.ok:
        return r.json()
    else:
        r.raise_for_status()


def query_by_name(url, params, name):
    params["query"] = "artist:" + name
    return query_site(url, params)


def pretty_print(data, indent=4):
    if type(data) == dict:
        print json.dumps(data, indent=indent, sort_keys=True)
    else:
        print data


def tag_extract(artist_tags):
    sorted(artist_tags, key=lambda tag: tag['count'], reverse=True)
    tag_list = []
    maxattrib = 5
    for index in range(len(artist_tags)):
        # look at either the first 5 tags or all the tags, let it be 5 for now
        if index > maxattrib:
            break
        tag_list.append(artist_tags[index]["name"])
    if len(tag_list) != maxattrib:
        tag_list = tag_list+[' ']*(maxattrib-len(tag_list))
    return tag_list


def main():
    fieldnames = ['LN', 'Artist', 'Count', 'Tag 1',
                  'Tag 2', 'Tag 3', 'Tag 4', 'Tag 5']
#    print fieldnames[6]
    with open('ArtistCounts.csv') as csvfile:
        ArtistReader = csv.reader(csvfile)
        outlist = []
        counter = 0
        # I know ahead of time this contains a header
        ArtistReader.next()
        for row in ArtistReader:
            # comment the next two lines later
            #            if counter == 100:
            #                break
            temp = list(row)
            artist = temp[1]
            try:
                results = query_by_name(
                    ARTIST_URL, query_type["simple"], artist)
                print
                artist_tags = results["artists"][0]["tags"]
                tag_list = tag_extract(artist_tags)
                outlist.append(temp+tag_list)
 #               with open('withTags.csv', 'wb') as outfile:
  #                  tagWriter =  csv.DictWriter(outfile, fieldnames=fieldnames )

   #                 if counter == 0:
    #                    tagWriter.writeheader()
     #                   print artist
      #              tagWriter.writerow({fieldnames[0] : str(temp[1]) ,
       #                                 fieldnames[1] : str(temp[2]),
        #                                fieldnames[2] : tag_list[0],
         #                               fieldnames[3] : tag_list[1],
          #                              fieldnames[4] : tag_list[2],
           #                             fieldnames[5] : tag_list[3],
            #                            fieldnames[6] : tag_list[4]}  )
            except:
                print "skipping line, no response from music brainz"
            counter += 1
            time.sleep(5)
            # if len(outlist) > 0:
            #   print outlist[len(outlist)-1]
        with open('withTags.csv', 'w') as outfile:
            tagWriter = unicodecsv.writer(
                outfile, quoting=csv.QUOTE_ALL, encoding='utf-8')
            tagWriter.writerow(fieldnames)
            for line in range(len(outlist)):
                tagWriter.writerow(outlist[line])


if __name__ == '__main__':
    main()
