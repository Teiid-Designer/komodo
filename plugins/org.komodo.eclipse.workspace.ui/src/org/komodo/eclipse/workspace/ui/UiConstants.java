package org.komodo.eclipse.workspace.ui;

import org.eclipse.jface.dialogs.IDialogConstants;
import org.eclipse.ui.IPageLayout;

public interface UiConstants {
	
    /**
     * The ID of the plug-in containing this constants class.
     * 
     * @since 4.0
     */
    String PLUGIN_ID = "org.komodo.eclipse.workspace.ui"; //$NON-NLS-1$
    

    String PACKAGE_ID = UiConstants.class.getPackage().getName();
    
    interface EditorIds {
        String KWORKSPACE_EDITOR = PLUGIN_ID + ".editor.kWorkspaceEditor"; //$NON-NLS-1$
    }
    
    /**
     * Keys for images and image descriptors stored in the image registry.
     * @since 4.0
     */
    interface Images {
        String ICONS = "icons/"; //$NON-NLS-1$
        String FULL = ICONS + "full/"; //$NON-NLS-1$
        
        String CTOOL16 = FULL + "ctool16/"; //$NON-NLS-1$
        String CVIEW16 = FULL + "cview16/"; //$NON-NLS-1$
        String DLCL16  = FULL + "dlcl16/"; //$NON-NLS-1$
        String ELCL16  = FULL + "elcl16/"; //$NON-NLS-1$
        String OBJ16   = FULL + "obj16/"; //$NON-NLS-1$
        String OVR16   = FULL + "ovr16/"; //$NON-NLS-1$
        String WIZBAN  = FULL + "wizban/"; //$NON-NLS-1$
        
        String KOMODO_ICON   = OBJ16 + "dragon_blue_16x16.png"; //$NON-NLS-1$
    }
    /**
     * Contains widget constants.
     * 
     * @since 4.0
     */
    interface Widgets extends IDialogConstants {
        /**
         * Contains private constants used by other constants within this interface.
         * 
         * @since 4.0
         */

        int IMAGE_ICON_GAP = 5;

        String OUR_NEXT_LABEL = "Next"; //$NON-NLS-1$
        String OUR_FINISH_LABEL = "Finish"; //$NON-NLS-1$

        String ADD_LABEL = "Add"; //$NON-NLS-1$
        String BROWSE_LABEL = "Browse..."; //$NON-NLS-1$
        String DESELECT_ALL_LABEL = "Deselect All"; //$NON-NLS-1$
        String DOWN_LABEL = "Down"; //$NON-NLS-1$
        String EDIT_LABEL = "Edit..."; //$NON-NLS-1$
        String REMOVE_LABEL = "Remove"; //$NON-NLS-1$
        String SELECT_ALL_LABEL= "Select All"; //$NON-NLS-1$
        String UP_LABEL = "Up"; //$NON-NLS-1$
        String NEW_LABEL = "New..."; //$NON-NLS-1$

        String PASSWORD_LABEL = "Password"; //$NON-NLS-1$

        String CONFIRM_MESSAGE_TITLE = "Confirm"; //$NON-NLS-1$
        String ERROR_MESSAGE_TITLE = "Error"; //$NON-NLS-1$
        String NOTIFICATION_MESSAGE_TITLE = "Notification"; //$NON-NLS-1$
        String WARNING_MESSAGE_TITLE = "Warning"; //$NON-NLS-1$

        String INVALID_FILE_MESSAGE = "Invalid File"; //$NON-NLS-1$
//        String VALID_DIALOG_MESSAGE = PC.getString("validDialogMessage", OK_LABEL); //$NON-NLS-1$
//        String VALID_LAST_OR_MIDDLE_PAGE_MESSAGE = PC.getString("validLastOrMiddlePageMessage", //$NON-NLS-1$
//                                                                OUR_NEXT_LABEL,
//                                                                OUR_FINISH_LABEL);
//        String VALID_LAST_PAGE_MESSAGE = PC.getString("validLastPageMessage", OUR_FINISH_LABEL); //$NON-NLS-1$
//        String VALID_PAGE_MESSAGE = PC.getString("validPageMessage", OUR_NEXT_LABEL); //$NON-NLS-1$

        String CHECKED_STATE_PROPERTY = "checkedState"; //$NON-NLS-1$

        int UNCHECKED = 1;
        int CHECKED = 1 << 1;
        int PARTIALLY_CHECKED = 1 << 2;
    }
    
    interface Extensions {
        // Perspectives
        String PERSPECTIVE = "komodoPerspective"; //$NON-NLS-1$
        String OUTPUT_FOLDER = PACKAGE_ID + ".outputFolder"; //$NON-NLS-1$
        String PROPERTY_FOLDER = PACKAGE_ID + ".propertyFolder"; //$NON-NLS-1$
        String TREE_FOLDER = PACKAGE_ID + ".treeFolder"; //$NON-NLS-1$
        String CENTER_LEFT_FOLDER = PACKAGE_ID + ".centerLeftFolder"; //$NON-NLS-1$
        String TOP_RIGHT_FOLDER = PACKAGE_ID + ".topRightFolder"; //$NON-NLS-1$
    }
    
    interface VIEWS {
        String OUTLINE_VIEW = IPageLayout.ID_OUTLINE;
        String PROPERTY_VIEW = IPageLayout.ID_PROP_SHEET;
    }
}
