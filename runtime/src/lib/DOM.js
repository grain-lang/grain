import { managedMemory } from '../runtime';
import { assertString, assertDOMElement, assertLambda, GRAIN_DOM_ELEM_TAG } from '../core/tags';
import { GRAIN_FALSE } from '../core/primitives';
import { grainHeapAllocate } from '../core/heap';
import { grainToJSVal } from '../utils/utils';

export const grainDOMRefs = [];

export function DOMQuery(n) {
  assertString(n);
  const view = managedMemory.view;
  let query = grainToJSVal(null, n);
  let elem = document.querySelector(query);
  if (elem) {
    grainDOMRefs.push(elem);
    let heapRef = grainHeapAllocate(2) / 4;
    view[heapRef] = GRAIN_DOM_ELEM_TAG;
    view[heapRef+1] = grainDOMRefs.length - 1;
    return (heapRef * 4) ^ 3;
  } else {
    return GRAIN_FALSE;
  }
}

export function DOMSetText(elemRef, textRef) {
  assertDOMElement(elemRef);
  assertString(textRef);
  let elem = grainToJSVal(null, elemRef);
  elem.innerText = grainToJSVal(null, textRef);
  return elemRef;
}

export function DOMDangerouslySetInnerHTML(elemRef, textRef) {
  assertDOMElement(elemRef);
  assertString(textRef);
  let elem = grainToJSVal(null, elemRef);
  elem.innerHTML = grainToJSVal(null, textRef);
  return elemRef;
}

export function DOMAddEventListener(elemRef, eventRef, handlerRef) {
  assertDOMElement(elemRef);
  assertString(eventRef);
  assertLambda(handlerRef);
  let elem = grainToJSVal(null, elemRef);
  let event = grainToJSVal(null, eventRef);
  let handler = grainToJSVal(null, handlerRef);
  elem.addEventListener(event, () => handler.call());
  return elemRef;
}
