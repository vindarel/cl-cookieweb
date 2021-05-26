(in-package :{{ cookiecutter.project_name }}/utils)


(defun format-date (date)
  "Format the given date with the default date format (yyyy-mm-dd). Return a string."
  (local-time:format-timestring nil date :format +date-y-m-d+))

(defun asciify (string)
  (str:downcase (slug:asciify string)))
