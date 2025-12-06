# Statement on Gen AI use

## Gen AI Chat Promopts

List of prompts/questions provided to ChatGPT:

- I'm making a shiny app and I want the app to be able to allow the user to upload an excel file. What functions can I use to do this?
- The time data is being read in the wrong format. It should be read in R as "POSIXct" "POSIXt". (I gave chat my current code reading the file to troubleshoot this)
- Next, I want to plot the data similar (if not the exact same) as I did in this script with the same data i gave you. The only caveat is that the script was written just for this specific experiment and is not modular. I want to be able to modify up to which row (aka time point) in the data to show, for the user to be able to specify which strains and conditions correspond to which well, and if there are replicates or not. (As you can see in this case, there was triplicates, with 4 different osmolalities and 5 different strains.) To make this an useful shiny app, i think the user should be able to indicate which wells belong to which strain or condition. Is it possible to make a 96 well (grid) layout appear on the shiny app, and for the user to highlight which wells are which strain (to plot separately), and if there are conditions (e.g. osmolality, hydrogen peroxide, etc., to be plotted together on the same plot).
  - With this prompt, I uploaded my .Rmd file that I use to plot experiment data in the lab.
- Some things to fix - I want the 96-well plate layout that comes up to be fixed 12 columns per row (not changing as the screen size changes, so that the layout is always the same as the 12 by 8 plate layout). Second, I want to be able to assign the conditions and strain one at a time, without needing both to be labelled at the same time. Lastly, I'd like each strain to be a different colour.
  - Follow up prompt: That made it worse, the buttons don't show up in the 12x8 format. Also, i want to be able to unclick a well if i accidentally clicked it.
- Right now my plot looks like this for 1 strain with 4 different conditions. I want the same strain to be plotted on the same plot (not facetted, but with just a legend). I only want separate strains to be facetted. (I provided a screenshot of my current plot)
- Ok also, instead of facetting and showing the plots for the different strains, i want to be able to click or swipe through the different strains so that only 1 plot is on the shiny app at one time (otherwise the plots would get too small to see!)
- Ok currently my page layout is like this, which pushes the plot way down as the table gets longer. I want the table to appear next to the 96 well plate buttons, and be the same length as the 96 well plate panel, turning into scroll as it gets longer.
- The well design table is too wide, and the 96 well panel needs to be wider to always show the 12 columns at once.
- Now the table doesn't scroll when it goes over the fixed length
- I want to add an option where if the user wants to use the app without their own data, they can use a demo dataset, which is uploaded to my github/environment. How do I do this? I want an option above 'Upload plate reader Excel file' where the user can just click "Use sample Excel file'
- I'd like to add an instructions panel above the current sidebar panel, explaining how to use this shinyapp. Where in my UI do I put this.

## Reflection

During this assignment, I used ChatGPT to figure out how to set up parts of my ShinyApp that I didn’t know how to do. Before GenAI, I would have googled the same questions and dug through blog posts or Stack Overflow, which would have taken much longer. With ChatGPT as a tool, I was able to take on a much more complicated project than I could have realistically completed on my own in the same time frame. One downside is that when the model gives me code that mostly works, I don’t always get as much practice troubleshooting or don't necessarily need to understand all the underlying details, so I have to be more intentional about going back to read and learn from the solutions.
