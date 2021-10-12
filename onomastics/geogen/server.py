from flask import Flask, request, Response
import geogen.client

app = Flask(__name__)


@app.route("/<name>/relative.svg")
def relative(name):
    color = request.args.get("color") or "navy"
    df = geogen.client.create_data_frame([name])
    return geogen.client.generate_map(df, "relative", fill_color=color)


@app.route("/<name>/data.csv")
def relative_csv(name):
    df = (
        geogen.client.create_data_frame([name])
        .drop(columns=["path"])
        .sort_values(by="absolute", ascending=False)
    )
    return Response(df.to_csv(index=False), mimetype="text/csv")


@app.route("/<name>/absolute.svg")
def absolute(name):
    color = request.args.get("color") or "navy"
    df = geogen.client.create_data_frame([name])
    return geogen.client.generate_map(df, "absolute", fill_color=color)


if __name__ == "__main__":
    app.run(host="0.0.0.0")
