use rodio::source::{SineWave, Source};
use rodio::{OutputStream, Sink};
use std::io;
use std::time::Duration;

fn main() {
    let (_stream, stream_handle) = OutputStream::try_default().unwrap();
    let sink = Sink::try_new(&stream_handle).unwrap();

    for input_line in io::stdin().lines() {
        if let [frequency_str, duration_str] = input_line
            .expect("Input empty?")
            .trim()
            .split_whitespace()
            .collect::<Vec<_>>()
            .as_slice()
        {
            let frequency: u32 = frequency_str.parse().expect("Input not an integer");
            let duration: u32 = duration_str.parse().expect("Input not an integer");

            let sound = SineWave::new(frequency as f32)
                .take_duration(Duration::from_millis(duration as u64));
            sink.append(sound);
        } else {
            panic!("Expected two integers: FREQUENCY DURATION")
        }
    }
    sink.sleep_until_end();
}
