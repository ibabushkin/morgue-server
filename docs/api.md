# Morgue Server API documentation
This document describes the available features and API calls of the server
application provided by `morgue-server`. It's main purpose is to allow a
distributed usage of the functionality provided by `morgue`.

All API calls are exposed through a RESTful interface as described below.

## Basic concept
Most API calls are verified using a so-called API key which can be assumed to
be unique for a user, and the user's name. Said API key can be obtained by a
separate request, after passing the necessary credentials.

All API calls are intended to be used over HTTP(S), and parameters are passed
in the request body via POST, encoded as JSON. All URIs are relative.

## JSON Primitives and Structure
All API calls are designed in such a way that their results can be reused as
parameter data on following calls. For this purpose (and to shorten the docs
significantly), some primitive definitions are applied, as well as a basic
structure for each response.

### Response Structure
Each response is a JSON object with two fields: `error` and `result`. 
The `error` field is of type `Error`, whereas `result` contains whatever
the docs of the corresponding API call tell you.

### JSON Primitives 
* `Error` - used to signal API errors:
  * `name`: `String` - describes the error

* `Credentials` - used for user creation and login: 
  * `name`: `String` - a user's name
  * `password`: `String` - a user's password

* `User` - represents a user, used for authentication:
  * `name`: `String` - a user's name
  * `api_key`: `String` - a user's API key

* `Group` - represents a group of users that share access to the group's files:
  * `name`: `String` - the group's name

* `File` - represents a file:
  * `name`: `String` - the filename
  * `content`: `String` - the file's content

* `Options` (Agenda) - settings for agenda generation:
  * `mode`: `String` - the agenda mode. One of "Timed", "Todo", "Both"
  * `double_spaces`: `Bool` - Double the amount of leading whitespace? Makes
    sense in case you use 2-space indent with markdown.
  * `tags`: `[String]` (optional) - tags to include
  * `skip_tags`: `[String]` (optional) - tags to skip
  * `num_days`: `Integer` - number of days for a timed agenda
  * `format`: `String` - the output format. One of "ANSI", "Plaintext", "Pango"
* `Options` (Outline) - settings for outline generation
  (the API decides based on context what kind of data you want):
  * `format`: `String` - the output format. One of "ANSI", "Plaintext", "Pango"

## Example
Every API call that accesses files, creates or modifies groups etc., needs a
user to be authorized against. This is the purpose of the (almost) omnipresent
`user`-parameter. Most other parameters should be self-explanatory.

## API Calls
Below is a list of all available API calls, and descriptions of their
parameters and return values.

### User-related

#### Creating a new User
* URI: `user/new`
* Parameters:
  * `Credentials`
* Return value `User`
* Errors
  * `EntityAlreadyExists` if the user already exists

Example:
```JSON
{ "name": "testUser"
, "password": "mypass"
}
```
might return either of these:
```JSON
{ "error": null
, "result": { "name": "testUser"
            , "api_key": "yourapikeyhere"
            }
}
```
```JSON
{ "error": { "name": "EntityAlreadyExists" }
, "result": null
}
```

#### Logging in as a User
API keys are persistent, however it is advisable to generate a new one in each
session.
* URI: `ùser/auth`
* Parameters: `Credentials`
* Return value: `User`
* Errors
  * `AuthError` if the credentials aren't valid

### Group-related
Groups of users are intended to share files amongst their members. That way,
notes can be edited, read, and processed in a colloborative fashion.

#### Creating a new Group
* URI: `group/new`
* Parameters
  * `user`: `User`
  * `groupname`: `String` - the name of the new group
* Return value: `Group`
* Errors
  * `AuthError` if the username / API key pair isn't valid
  * `EntityAlreadyExists` if a group with that name is existing already

Example:
```JSON
{ "user": { "name": "testUser"
          , "api_key": "yourapikeyhere"
          }
, "groupname": "testGroup"
}
```
would result in the following response if no errors occur.
```JSON
{ "error": null
, "result": { "name": "testGroup" }
}
```

#### Add a User to a Group
* URI: `group/add`
* Parameters
  * `user`: `User`
  * `group`: `Group` - the group to be added to
  * `username`: `String` - the user to be added
* Return value: `Group`
* Errors
  * `AuthError` if the username / API key pair isn't valid
  * `NoAccess` if the calling user isn't member of the group
  * `NoSuchEntity` if the user to be added isn't present
  * `ÈntityAlreadyExists` if the user to be added is already a member

#### List all users in a Group
* URI: `group/members`
* Parameters:
  * `user`: `User`
  * `group`: `Group`
* Return value: `[String]`
* Errors
  * `AuthError` if the username / API key pair isn't valid
  * `NoSuchEntity` if the given group doesn't exist

### File-related
To distinguish between files in a user's own store and available to
him through group membership, a path-like syntax is used, which is shared
across all file-related queries once the system has a notion of them. Thus,
all API calls except `file/push` use a path-like syntax. For instance, a file
named `test`, owned by user `testUser` is represented as `u/testUser/test`, and
a file owned by group `testGroup` with the same name is represented as
`g/testGroup/test`.

#### List available files
List all files that the user can access, both his own and of groups he is a
member of.
* URI: `file/browse`
* Parameters: `User`
* Return value:
  * `name`: `String` - the user's name
  * `files`: `[File]` - all available files
* Errors
  * `AuthError` if the username / API key pair isn't valid

#### Get a file's content
* URI: `file/pull`
* Parameters
  * `user`: `User`
  * `filename`: `String`
* Return value: `File`
* Errors
  * `AuthError` if the username / API key pair isn't valid
  * `NoAccess` if the user is not allowed to access the given file

#### Upload a file
Files are uploaded to the private store by default, unless the `group`
parameter is given. In that case, it is uploaded to the group's store.
* URI: `file/push`
* Parameters
  * `user`: `User`
  * `group`: `Group` (optional) - the group to upload to
  * `file`: `File` 
* Return value: `String` - the filepath
* Errors
  * `AuthError` if the username / API key pair isn't valid
  * `EntityAlreadyExists` if the file exists already
  * `NoAccess` if a group is given and the user isn't a member

### Agenda-related

#### Generate an Agenda or Outline by passing settings
* URI: `morgue`
* Parameters
  * `user`: `User`
  * `options`: `Options` - options to generate an agenda or outline
  * `files`: `[String]` - files to generate the agenda/outline for
* Return value: `String` - the agenda
* Errors
  * `AuthError` if the username / API key pair isn't valid
  * `NoAccess` if the user is not allowed to access the given file(s)
