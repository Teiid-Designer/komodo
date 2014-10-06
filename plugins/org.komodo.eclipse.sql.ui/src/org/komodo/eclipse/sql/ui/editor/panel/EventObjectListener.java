/*
 * JBoss, Home of Professional Open Source.
 *
 * See the LEGAL.txt file distributed with this work for information regarding copyright ownership and licensing.
 *
 * See the AUTHORS.txt file distributed with this work for a full listing of individual contributors.
 */

package org.komodo.eclipse.sql.ui.editor.panel;

import java.util.EventListener;
import java.util.EventObject;

/**
 * @since 8.0
 */
public interface EventObjectListener extends EventListener {
    void processEvent( EventObject obj );
}
