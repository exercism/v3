/**
 * These are the shapes of the external grocer service', the return values and the
 * functions. Don't change these. In various IDEs, such as vscode, this will add
 * type information on the fly
 */

interface CheckStatus {
  callback: StatusCallback;
}

type StatusCallback = (response: string) => boolean;

interface CheckInventory {
  query: GrocerQuery;
  callback: InventoryCallback;
}

type GrocerQuery = {
  variety: string;
  quantity: number;
};

type InventoryCallback = (err: string | null, isAvailable: boolean) => string;
