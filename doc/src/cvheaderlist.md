# cv headaer list

A simple tool that reads a binary _cv_ file and lists relevant header data.

## Usage
```shell
user@localhost:~$ cv_header_list -h

NAME
        cv_header_list - list cv header 

USAGE
        cv_header_list -i <cv-file>
                    [ -h ]

DESCRIPTION
        Script to start new LBC processing suite

OPTIONS

        -i cv-file
           cv file

        -h Help! Print usage information.

user@localhost:~$ 

```

## Example
```shell
user@localhost:~$ cv_header_list -i /home/ewhelan/stab_40h1_IRELAND25_064_480.cv

 JPDAYS       ::          480
 SW(lon-lat)  ::   -19.520623099467286        42.575525141583732
 NE(lon-lat)  ::    4.3664253929474990        63.303769438767112
 REF(lon-lat) ::    5.0000001489527870        53.500001593794821
 NDGL         ::          800
 NDLON        ::          800
 NDGUX        ::          789
 NDLUX        ::          789
 NSMAX        ::          399
 NMSMAX       ::          399
 NFLEVG       ::           65

user@localhost:~$ 
```


