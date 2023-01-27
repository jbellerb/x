use anyhow::Result;
use image::{Rgb, RgbImage};

pub fn upscale(img: RgbImage, width: usize, height: usize) -> Result<RgbImage> {
    Ok(RgbImage::from_fn(width, height, |x, y| {
        let u = (x as f64) / 5906.0 * 63.0 - 0.5;
        let v = (y as f64) / 5906.0 * 63.0 - 0.5;

        let x = u as u32;
        let y = v as u32;

        let texel_u = u - (x as f64);
        let texel_v = v - (y as f64);

        let c00 = img[(62.min(x + 0), 62.min(y + 0))];
        let c01 = img[(62.min(x + 0), 62.min(y + 1))];
        let c10 = img[(62.min(x + 1), 62.min(y + 0))];
        let c11 = img[(62.min(x + 1), 62.min(y + 1))];

        if texel_u - texel_v > 0.0 {
            let (r00, g00, b00) = (c00[0] as f64, c00[1] as f64, c00[2] as f64);
            let (r10, g10, b10) = (c10[0] as f64, c10[1] as f64, c10[2] as f64);
            let (r11, g11, b11) = (c11[0] as f64, c11[1] as f64, c11[2] as f64);

            Rgb([
                (r10 + (1.0 - texel_u) * (r00 - r10) + texel_v * (r11 - r10)) as u8,
                (g10 + (1.0 - texel_u) * (g00 - g10) + texel_v * (g11 - g10)) as u8,
                (b10 + (1.0 - texel_u) * (b00 - b10) + texel_v * (b11 - b10)) as u8,
            ])
        } else {
            let (r00, g00, b00) = (c00[0] as f64, c00[1] as f64, c00[2] as f64);
            let (r01, g01, b01) = (c01[0] as f64, c01[1] as f64, c01[2] as f64);
            let (r11, g11, b11) = (c11[0] as f64, c11[1] as f64, c11[2] as f64);

            Rgb([
                (r01 + texel_u * (r11 - r01) + (1.0 - texel_v) * (r00 - r01)) as u8,
                (g01 + texel_u * (g11 - g01) + (1.0 - texel_v) * (g00 - g01)) as u8,
                (b01 + texel_u * (b11 - b01) + (1.0 - texel_v) * (b00 - b01)) as u8,
            ])
        }
    }))
}
