/* TypeScript file generated from Extras__TaskResult.resi by genType. */
/* eslint-disable import/first */


import type {t as Extras__Task_t} from './Extras__Task.gen';

// tslint:disable-next-line:interface-over-type-literal
export type t<ok,err> = Extras__Task_t<
    { tag: "Ok"; value: ok }
  | { tag: "Error"; value: err }>;
