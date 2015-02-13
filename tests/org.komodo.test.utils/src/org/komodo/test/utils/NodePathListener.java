/*
 * JBoss, Home of Professional Open Source.
 * See the COPYRIGHT.txt file distributed with this work for information
 * regarding copyright ownership.  Some portions may be licensed
 * to Red Hat, Inc. under one or more contributor license agreements.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301 USA.
 */
package org.komodo.test.utils;

import static org.junit.Assert.fail;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import javax.jcr.observation.EventIterator;
import javax.jcr.observation.EventListener;
import org.komodo.utils.KLog;
import org.modeshape.jcr.api.observation.Event;

/**
 * Listener that checks new nodes to see if they equals given paths
 */
public class NodePathListener implements EventListener {

    private final List<String> paths = new ArrayList<String>();

    private final int pathsSize;

    private final CountDownLatch updateLatch;

    /**
     * @param paths paths to look for during sequencing
     * @param updateLatch countdown latch to change when paths are found
     */
    public NodePathListener(List<String> paths, CountDownLatch updateLatch) {
        this.paths.addAll(paths);
        this.pathsSize = paths.size();
        this.updateLatch = updateLatch;
    }

    @Override
    public void onEvent(EventIterator events) {
        while (events.hasNext()) {
            try {
                Event event = (Event)events.nextEvent();

                String nodePath = event.getPath();

                Iterator<String> pathIter = paths.iterator();
                while (pathIter.hasNext()) {
                    String path = pathIter.next();

                    if (nodePath.matches(path))
                        pathIter.remove();
                }

            } catch (Exception ex) {
                fail(ex.getMessage());
            }
        }

        if (paths.isEmpty() && updateLatch.getCount() > 0) {
            KLog.getLogger().info("All paths have been found. Setting latch to continue: Passed"); //$NON-NLS-1$

            for (int i = 0 ; i < pathsSize; ++i) {
                updateLatch.countDown();
            }
        }

    }
}