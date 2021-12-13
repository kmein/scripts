import astral
import astral.sun
import click
import datetime
import matplotlib.dates
import matplotlib.pyplot
import operator
import sys
import timezonefinder


def days_between(start: datetime.datetime, end: datetime.datetime):
    delta = end - start
    for offset in range(delta.days):
        yield start + datetime.timedelta(days=offset)


tf = timezonefinder.TimezoneFinder()


def sun_for(day: datetime.datetime, *, latitude: float, longitude: float):
    observer = astral.Observer(latitude, longitude)
    return astral.sun.sun(
        observer, date=day, tzinfo=tf.timezone_at(lat=latitude, lng=longitude)
    )


def time_to_float(time: datetime.datetime) -> float:
    return time.hour + (time.minute / 60)


@click.command()
@click.option(
    "--start",
    default=datetime.datetime.today(),
    type=click.DateTime(),
    help="Start date",
)
@click.option(
    "--end",
    default=datetime.datetime.today() + datetime.timedelta(days=365),
    type=click.DateTime(),
    help="End date",
)
@click.option("--latitude", type=float, default=52.52, help="Observer latitude")
@click.option("--longitude", type=float, default=13.4, help="Observer longitude")
def main(start, end, latitude, longitude):
    print("Using", tf.timezone_at(lat=latitude, lng=longitude), file=sys.stderr)

    days = list(days_between(start, end))
    sun_info = [sun_for(day, latitude=latitude, longitude=longitude) for day in days]

    sunrises = [time_to_float(sun["sunrise"]) for sun in sun_info]
    sunsets = [time_to_float(sun["sunset"]) for sun in sun_info]
    sun_hours = [set - rise for set, rise in zip(sunsets, sunrises)]

    matplotlib.pyplot.gca().xaxis.set_major_formatter(
        matplotlib.dates.DateFormatter("%d.%m.")
    )
    matplotlib.pyplot.gca().xaxis.set_major_locator(
        matplotlib.dates.DayLocator(interval=30)
    )
    matplotlib.pyplot.plot(days, sunrises)
    matplotlib.pyplot.plot(days, sunsets)
    matplotlib.pyplot.plot(days, sun_hours)
    matplotlib.pyplot.gcf().autofmt_xdate()
    matplotlib.pyplot.savefig(sys.stdout, format="svg")


if __name__ == "__main__":
    main()
