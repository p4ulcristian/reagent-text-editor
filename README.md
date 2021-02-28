# reagent-text-editor

I needed a rich-text-editor (rte) which is not dependent on the "obsolete" execCommand. (https://developer.mozilla.org/en-US/docs/Web/API/Document/execCommand)
So I decided to write one myself. This is only for demo purposes, trying to make my dreams come true.
Inspiration came from: https://www.youtube.com/watch?v=feUYwoLhE_4
I also use reagent-template: https://github.com/reagent-project/reagent-template

## Development mode

To start the Figwheel compiler, navigate to the project folder and run the following command in the terminal:

```
lein figwheel
```

## How it works?

  1. We have ```[:div {:content-editable true}]```
  2. We have two states, cursor-state and editor-state
  4. To be continued...
