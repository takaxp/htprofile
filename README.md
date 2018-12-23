# Usage
1. Start logging by `M-x htprofile-start`
2. Show data by one of the following
    - Show statistics by `M-x htprofile-show-statistics`
    - Show log data in a raw form by `M-x htprofile-show-log`

# Customization
```elisp
(setq htprofile-sort-by 'total-time) ; one of 'max-time (default), 'total-time, 'average-time

(setq htprofile-max-log 5000) ; default: 1000

(setq htprofile-float-format 'sec) ; default: 'msec
(setq htprofile-float-format (list :width 9
                                   :precision 6
                                   :multiplier 1))
```
