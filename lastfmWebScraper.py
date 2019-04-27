import bs4
import requests
import csv
import time
import unicodecsv
#response = requests.get("http://www.last.fm/music/Dr+Dog")
# print response.text
#soup = bs4.BeautifulSoup(response.text,'html.parser')
# print(soup.prettify())
# tags=soup.find_all(rel="tag")


def webscrape(artistname):
    fmtArtName = "+".join(artistname.split(' '))
    try:
        print "requesting http://www.last.fm/music/"+fmtArtName
        response = requests.get("http://www.last.fm/music/"+fmtArtName)
    except:
        print "error in Last.fm"
        return []
    soup = bs4.BeautifulSoup(response.text, 'html.parser')
    tags = soup.find_all(rel="tag")
    tag_list = []
    maxattrib = 5
    index = 0
    for tag in tags:
        # print tag.get_text()
        if index == maxattrib:
            break
        tag_list.append(tag.get_text())
        index += 1
        # endfor
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
        ArtistReader.next()
        for row in ArtistReader:
            # comment the next two lines later
            #            if counter == 10:
            #                break
            temp = list(row)
            artist = temp[1]
            outlist.append(temp+webscrape(artist))
            counter += 1

    with open('lastfm.csv', 'w') as outfile:
        tagWriter = unicodecsv.writer(
            outfile, quoting=csv.QUOTE_ALL, encoding='utf-8')
        tagWriter.writerow(fieldnames)
        for line in range(len(outlist)):
            tagWriter.writerow(outlist[line])


if __name__ == '__main__':
    main()
