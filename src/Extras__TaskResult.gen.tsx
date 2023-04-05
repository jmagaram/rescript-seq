/* TypeScript file generated from Extras__TaskResult.resi by genType. */
/* eslint-disable import/first */


// tslint:disable-next-line:interface-over-type-literal
export type t<ok,err> = Task_t<
    { tag: "Ok"; value: ok }
  | { tag: "Error"; value: err }>;
