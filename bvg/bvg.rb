#!/usr/bin/env ruby

require "open-uri"
require "nokogiri"
require "rainbow"

Departure = Struct.new(:time, :line, :terminus) do
  def self.from_row(row)
    Departure.new(row[0].text.strip, row[1].text.strip.gsub(/\s+/, " "), row[2].text.strip)
  end

  def log
    time_string = if time[-1] == "*" then Rainbow(time[0..4]).inverse else time end
    line_string = {
      'B' => Rainbow(line).magenta,
      'R' => Rainbow(line).white,
      'U' => Rainbow(line).blue,
      'S' => Rainbow(line).green,
      'T' => Rainbow(line).red,
    }[line[0]] || line

    printf("%s %-20s %s\n", time_string, line_string, terminus)
  end
end

def request_url(input)
  "http://mobil.bvg.de/Fahrinfo/bin/stboard.bin/dox?" + URI.encode_www_form(
    "input" => URI.encode_www_form_component(input),
    "start" => "yes",
    "boardType" => "depRT",
  )
end

url = request_url(ARGV[0])

document = Nokogiri::HTML(open(url))

title = document.xpath("//div[@id=\"ivu_overview_input\"]/strong")
puts Rainbow(title.text).bright if title

document.xpath("//span[@class=\"select\"]/a").each do |option|
  station_name = option.text.strip
  station_code = option.xpath("@href").text[/input=(\d+)/, 1]
  print station_code, " ", Rainbow(station_name).yellow, "\n"
end

document.xpath("//table/tbody/tr").each do |row|
  columns = row.xpath("td")
  Departure.from_row(columns).log unless columns.length < 3
end
