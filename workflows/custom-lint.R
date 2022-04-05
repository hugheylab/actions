library('data.table')
library('glue')
library('lintr')
library('rex')

if (!exists('repository')) {
  dirIndexes = gregexpr('/', getwd())[[1]]
  repository = substr(getwd(), dirIndexes[length(dirIndexes)] + 1, nchar(getwd()))}
if (!exists('branch')) branch = 'main'

double_quotes_linter = function(source_file) {
  content = source_file$full_parsed_content
  str_idx = which(content$token == 'STR_CONST')
  squote_matches = which(re_matches(
    content[str_idx, 'text'],
    rex(start, double_quote, any_non_single_quotes, double_quote, end)
  ))

  lapply(
    squote_matches,
    function(id) {
      with(content[str_idx[id], ], {
        line = source_file$file_lines[line1]
        col2 = if (line1 == line2) col2 else nchar(line)
        Lint(
          filename = source_file$filename,
          line_number = line1,
          column_number = col1,
          type = 'style',
          message = 'Only use single-quotes.',
          line = line,
          ranges = list(c(col1, col2)))})})}

newDefaults = with_defaults(
  assignment_linter = NULL,
  closed_curly_linter = NULL,
  commented_code_linter = NULL,
  cyclocomp_linter = NULL,
  double_quotes_linter = double_quotes_linter,
  line_length_linter(120),
  object_name_linter = object_name_linter('camelCase'),
  open_curly_linter = open_curly_linter(TRUE),
  single_quotes_linter = NULL)

getLintDt = function(lintsFound, repository = NULL, branch = NULL) {
  if (is.null(repository)) {
    dirIndexes = gregexpr('/', getwd())[[1]]
    repository = substr(getwd(), dirIndexes[length(dirIndexes)] + 1, nchar(getwd()))}

  if (is.null(branch)) branch = 'main'

  lfDt = unique(as.data.table(lintsFound), by = c('filename', 'line_number', 'message'))

  lfDt[, lint_link := glue(
    'https://github.com/hugheylab/{repository}/blob/{branch}/{filename}#L{line_number}',
    .envir = .SD)]
  lfDt[, line := trimws(line)]
  # lfDt[, line := gsub('\\\\r', '', line)]
  # lfDt[, line := gsub('\\\\n', '', line)]
  # lfDt[, line := gsub('\\r', '', line)]
  # lfDt[, line := gsub('\\n', '', line)]
  setorder(lfDt, filename, line_number)

  lintExcludeFilename = 'lint_exclude.csv'
  if (file.exists(lintExcludeFilename)) {
    lintExclude = fread(lintExcludeFilename)
    onCols = c('filename', 'line_number', 'message', 'line')
    lintExcludeNotFound = lintExclude[!lfDt, on = onCols]
    if (nrow(lintExcludeNotFound) > 0) {
      warning('There are lines in the exclusion not found in the current code base.')}
    lfDt = lfDt[!lintExclude, on = onCols]}

  # %0D = \r and %0A = \n
  # Needs different formatting for bash output
  lfDt[, format_line :=
         sprintf('%s. %s line %s: %s (%s) \r\n    ```r \r\n    %s  \r\n    ```',
              .I, filename, line_number, message, lint_link, line)]
  return(lfDt)}

getFormattedIssueStr = function(lfDt) {
  newlineEsc = ' \r\n'
  issueStr = paste0(lfDt$format_line, collapse = newlineEsc)
  # issueStr = gsub('"', '%22', issueStr, fixed = TRUE)
  # issueStr = gsub("'", "'\"'\"'", issueStr, fixed = TRUE)
  fileName = 'temp_csv.csv'
  fwrite(lfDt[, .(filename, line_number, message, line)], fileName)
  tempCsvStr = readChar(fileName, file.info(fileName)$size)
  unlink(fileName)
  issueStr = paste0(
    issueStr,
    newlineEsc,
    'To exclude any of these issues from being detected, add the appropriate ',
    'lines, shown below, to lint_exclusions.csv at the top-level of the repository:',
    newlineEsc,
    '```',
    newlineEsc,
    tempCsvStr,
    newlineEsc,
    '```')
  lintExcludeFilename = 'lint_exclude.csv'
  if (file.exists(lintExcludeFilename)) {
    lintExclude = fread(lintExcludeFilename)
    onCols = c('filename', 'line_number', 'message', 'line')
    lintExcludeNotFound = lintExclude[!lfDt, on = onCols]
    if (nrow(lintExcludeNotFound) > 0) {
      lintExcludeNotFound[, format_line :=
                            sprintf('%s line %s: %s \r\n    ```r \r\n    %s  \r\n    ```',
                                    filename, line_number, message, line)]
      notFoundStr = paste0(lintExcludeNotFound$format_line, collapse = '\r\n')
      issueStr = paste0(
        issueStr,
        newlineEsc,
        'Warning, there are lines in the exclusion not found in the current code base: ',
        newlineEsc,
        notFoundStr)}}
  return(issueStr)}

lintsFound = lint_dir(linters = newDefaults, pattern = rex('.', or(one_of('Rr'), 'Rmd'), end))
lintsFound
