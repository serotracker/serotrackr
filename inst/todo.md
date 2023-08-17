# List of TODOs:

## General
- Add another function to evaluate assays, including their cutoff values.
- `regions` and `regions_df` need more cleaning. Some `adm1` values are missing. Some `adm1` and `adm2` values have garbled text. The manually-cleaned data still have issues and could not be merged with the regions data for now. More explanation is [here](https://github.com/serotracker/serotrackr/blob/main/data-raw/regions.R).

## st_validate()

- Recognize character `"na"` or `"n/a"` values as `NA` in character columns.
- Add feature to generate age_group, if missing, based on age.
- Separate validation of character-type and date-type start or end date columns for efficiency. Currently, `start_date` and `end_date` columns are all first converted to character, even if they are provided as date-type by the user. This was done to simplify the validation process.
- Fix `result_cat`'s output to be sentence case. Currently, it only checks if the input is `positive`, `borderline`, or `negative`, regardless of case, however, the output is not made consistent. For example, the output could contain both `poSitive` and `positivE` values!

## st_locate()
- The ... argument currently does not check if hierarchy of adm1 and adm2 is valid. `st_locate()` only checks that in its `automatic` section (when performing left_join).
- The ... argument must run only if adm1 and/or adm2 are column names. Temporary fix is applied. Improve this.
- `st_locate()`'s reporting message can be confusing when an adm1 region has errors, its corresponding adm2 is valid, and there are other erroneous adm2 values. This is because adm1 must be valid before evaluating adm2 values. In this case, the user currently needs to run `st_locate()` twice, first to address adm1, and then to address adm2. Improve this process.

## st_aggregate()
- Check for validation rules to make sure the validated data from `st_validate()` has not been altered by user.
- Add support for multiple tests and test combinations.
- Print a message about dropping any `NAs`.
- Dropping NAs for `result` and `result_cat` should be done at different stages, as they are used to calculate different columns: `result` is used for generating columns containing `titer` in their name; `result_cat` is used for generating seroprevalence estimates. Currently, the `NAs` for all of `dataset_id`, `id`, `result`, and `result_cat` columns are dropped at the same time, while the NA patters may not be similar across them.
- When there's only one `age_group` or `sex`, `st_aggregate` should not generate `age_group` or `sex` subgroups, respectively.

## st_save()
- Check for validation rules to make sure the validated data from `st_validate()` has not been altered by user.
- Check data and estimates have the same object ID.
- Add column names for `other columns` in the `Data` sheet.
- The `number_people` field in the `Study Metadata` sheet  should be length of unique IDs, not just `nrow(data)`.