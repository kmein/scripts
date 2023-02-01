from datetime import date, timedelta
import sys

epoch_begin = date(2020, 3, 15)
epoch_end = date(2022, 3, 30)
days_within_epoch = epoch_end - epoch_begin


if __name__ == "__main__":
    if len(sys.argv[1:]) < 1:
        exit(1)
    start_date = date.fromisoformat(sys.argv[1])

    days_before_epoch = epoch_begin - start_date
    days_after_epoch = date.today() - epoch_end

    days_outside_epoch = days_before_epoch + days_after_epoch

    difference = days_within_epoch - (days_outside_epoch)

    print("Days inside epoch:", days_within_epoch)
    print("Days outside epoch:", days_outside_epoch)
    print("Breakeven", date.today() + difference)
