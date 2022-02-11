# AUTOTYP (in progress)

- Fixed the DOI badge (now points to last released version 1.0.0)
- Added data type `logical` to the list of valid variable types
- Clarified that `value-list` is not actually a list
- Fixed an issue with JSON export where missing values were silently dropped
  by the serializer, they are now exported as `null` 
