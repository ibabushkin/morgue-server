# Morgue Server API documentation
This document describes the available features and API calls of the server
application provided by `morgue-server`. It's main purpose is to allow a
distributed usage of the functionality provided by `morgue`.

All API calls are exposed through a REST-ish interface as described below.

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
  * `members`: `[String]` - the members' names

* `File` - represents a file:
  * `name`: `String` - the filename
  * `content`: `String` - the file's content

* `FileList` - represents a set of files belonging to a user,
  as well as a set of groups.
  * `user_files`: `[String]` - the names of files owned by the calling user
  * `group_files`: `[GroupFileList]` - a list of structures representing
    groups and the corresponding files.

* `GroupFileList` - represents a set of files belonging to a group
  * `group`: `String` - the group's name
  * `files`: `[String]` - the names of files owned by that group

* `Options` (Agenda) - settings for agenda generation:
  * `mode`: `String` - the agenda mode. One of `Timed`, `Todo`, `Both`
  * `double_spaces`: `Bool` - Double the amount of leading whitespace? Makes
    sense in case you use 2-space indent with markdown.
  * `tags`: `[String]` (optional) - tags to include
  * `skip_tags`: `[String]` (optional) - tags to skip
  * `num_days`: `Integer` - number of days for a timed agenda
  * `format`: `String` - the output format. One of `ANSI`, `Plaintext`, `Pango`
* `Options` (Outline) - settings for outline generation
  (the API decides based on context what kind of data you want):
  * `format`: `String` - the output format. One of `ANSI`, `Plaintext`, `Pango`

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
  * `UserExists` if the user already exists

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

#### Upload a file
* URI: `user/push`
* Parameters
  * `user`: `User`
  * `file`: `File` 
* Return value: `String` - the filename
* Errors
  * `AuthError` if the username / API key pair isn't valid
  * `FileExists` if the file exists already

#### Get a file's content
* URI: `user/pull`
* Parameters
  * `user`: `User`
  * `filename`: `String`
* Return value: `File`
* Errors
  * `AuthError` if the username / API key pair isn't valid
  * `NoSuchFile` if the given file does not exist

#### Patch a file
* URI `user/patch`
* Parameters
  * `user`: `User`
  * `filename`: `String`
  * `patch`: `String` - a patch as accepted by the tool `patch`.
* Return value: `File`
* Errors
  * `AuthError` if the username / API key pair isn't valid
  * `NoSuchFile` if the given file does not exist

#### List available files
List all files that the user can access, both his own and of groups he is a
member of.
* URI: `user/list`
* Parameters: `User`
* Return value:
  * `files`: `FileList` - all available files
* Errors
  * `AuthError` if the username / API key pair isn't valid

#### Generate an Agenda or Outline by passing settings
* URI: `user/agenda`
* Parameters
  * `user`: `User`
  * `options`: `Options` - options to generate an agenda or outline
  * `files`: `FileList` - files to generate the agenda/outline for
* Return value: `String` - the agenda
* Errors
  * `AuthError` if the username / API key pair isn't valid
  * `NoAccess` if the user is not allowed to access the given file(s)

### Group-related
Groups of users are intended to share files amongst their members. That way,
notes can be edited, read, and processed in a distributed manner.

#### Creating a new Group
* URI: `group/new`
* Parameters
  * `user`: `User`
  * `groupname`: `String` - the name of the new group
* Return value: `Group`
* Errors
  * `AuthError` if the username / API key pair isn't valid
  * `GroupExists` if a group with that name is existing already

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
, "result": { "name": "testGroup"
            , "members": []
            }
}
```

#### Add a User to a Group
* URI: `group/add`
* Parameters
  * `user`: `User`
  * `group`: `String` - the group's name to be added to
  * `username`: `String` - the user's name to be added
* Return value: `Group`
* Errors
  * `AuthError` if the username / API key pair isn't valid
  * `NoAccess` if the calling user isn't member of the group
  * `NoSuchUser` if the user to be added isn't present
  * `MemberExists` if the user to be added is already a member

#### Upload a file
* URI: `group/push`
* Parameters
  * `user`: `User`
  * `group`: `String`
  * `file`: `File` 
* Return value: `String` - the filename
* Errors
  * `AuthError` if the username / API key pair isn't valid
  * `FileExists` if the file exists already
  * `NoAccess` if the user isn't member of the group

#### Get a file's content
* URI: `group/pull`
* Parameters
  * `user`: `User`
  * `group`: `String`
  * `filename`: `String`
* Return value: `File`
* Errors
  * `AuthError` if the username / API key pair isn't valid
  * `NoSuchFile` if the given file does not exist
  * `NoAccess` if the user isn't member of the group

#### Patch a file
* URI `group/patch`
* Parameters
  * `user`: `User`
  * `group`: `String`
  * `filename`: `String`
  * `patch`: `String` - a patch as accepted by the tool `patch`.
* Return value: `File`
* Errors
  * `AuthError` if the username / API key pair isn't valid
  * `NoAccess` if the user isn't member of the group
  * `NoSuchFile` if the given file does not exist
