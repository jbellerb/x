use n64scale::upscale;

use anyhow::Result;

fn main() -> Result<()> {
    let img = image::open("jrb.png")?.into_rgb8();

    let upscale = upscale(img)?;

    upscale.save("out.png")?;

    Ok(())
}
