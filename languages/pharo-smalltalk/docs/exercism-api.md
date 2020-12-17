# Exercism API

## Description

Most Exercism language tracks use the `exercism` command line tool for fetching and submitting exercises which interacts with `exercism.io` via a REST API. The Pharo Smalltalk language track instead uses the same REST API to bring exercises in and out of the Pharo image without the use of any intermediary tooling. This document details the Exercism API with a focus on how it is used by the Pharo Smalltalk language track. It is intended for maintainers of the Pharo Smalltalk tooling.

## Headers

All Endpoints require headers on the request to be set to the following values:

```
Authorization: Bearer <token>
```

The value `<token>` is the users CLI authentication token available at `https://exercism.io/my/settings`.

## Fetching Exercises

```
Content-Type: application/json
```

`GET api.exercism.io/v1/solutions/latest?exercise_id=<slug>&track_id<language> HTTP/1.1`

In the URL above `<slug>` is the name of the exercise e.g. `hello-world` and `<language>` is the language track e.g. `pharo-smalltalk`.

### Response

#### 200

A sucessful request will send a response containing a JSON payload. Inside the JSON will be information on the exercise and a url from which to download the exercise files.

```json
"solution":{
	"id":"<id-string>",
	"url":"https://exercism.io/my/solutions/<id-string>",
	"team":null,
	"user":{
		"handle":"<exercism-user-name>",
		"is_requester":true
		},
		"exercise":{
			"id":"isogram",
			"instructions_url":"https://exercism.io/my/solutions/<id-string>",
			"auto_approve":false,
			"track":{
				"id":"pharo-smalltalk",
				"language":"Pharo"
				}
			},
			"file_download_base_url":"https://api.exercism.io/v1/solutions/<id-string>/files/",
			"files":[
				"IsogramTest.class.st",
				"README.md"
				],
			"iteration":null
		}
	}
```

#### 400

The language track name is ambiguous, or the submitted exercise files have not changed.

#### 403

The solution not unlocked for the user or the language track is not joined.

#### 404

The language track or exercise could not be found.

## Submitting Exercises

```
Content-Type: application/octet-stream
```

`PATCH api.exercism.io//v1/solutions/<some-kind-of-token-or-hash> HTTP/1.1`

### Response

#### 201

A successful submission.

#### 400

A submitted file is too large (greater than one megabyte), or the solution is a duplicate.

#### 403

The solution is not accessable to the user. The solution user ID and the current user ID don't match.

#### 404

The solution ID was not found.

## Reference

- [exercism/pharo-smalltalk issue-32](https://github.com/exercism/pharo-smalltalk/issues/32)
- [exercism/exercism issue-4087](https://github.com/exercism/exercism/issues/4087)
- Refer to download and submit tests

