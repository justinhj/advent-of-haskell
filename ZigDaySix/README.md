# Zig notes

This was written by me and a comvination of Claude, Bing Copilot, Deepseek and ChatGPT.
None of us are great at Zig. Also Zig 0.13.0 is not well covered in the models.

Built with `zig build -Doptimize=ReleaseFast` it runs in about ~~3.5~~ 1.5 seconds compared with 10 seconds for the Haskell version.

Note I have change the algorithm on the Zig version which took it from 3.5 to 1.5 seconds. Instead of tracking visited blocks in a hashmap I do the old trick of having two iterators through the path, one at normal speed and the other at double speed. If they catch up then there is a loop.

Next optimization was to use a number of threads and run them in parallel. As shown below this worked best at 4 threads then showed diminishing returns. A part of this optimization was to give each thread its own memory allocator to avoid contention.

Image link to images/Figure_1.png
<img src="images/Figure_1.png" alt="drawing" width="400"/>


