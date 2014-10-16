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
package org.komodo.eclipse.runtime.client.driver.provider;

import java.io.File;
import org.eclipse.core.runtime.IPath;
import org.eclipse.datatools.connectivity.services.PluginResourceLocator;
import org.komodo.spi.constants.StringConstants;

/**
 *
 */
public class PluginFinder implements StringConstants {

    /**
     * Find the plugin jar based on the given path
     *
     * @param path
     * @return
     */
    private String findPluginJar(IPath path) {
        if (path == null)
            return null;

        if (!path.getFileExtension().equals(JAR))
            path = path.addFileExtension(JAR);

        String osPath = path.toOSString();
        File jarFile = new File(osPath);
        if (jarFile.exists())
            return osPath;

        return null;
    }

    /**
     * Find a built jar in a 'target' sub-directory of the given path location.
     *
     * This location only really occurs when developing / testing and a built
     * version is available in the maven target directory.
     *
     * @param path
     * @param pluginId
     * @return
     */
    private String findTargetJar(IPath path, String pluginId) {
        if (path == null)
            return null;

        path = path.append(TARGET);

        File targetDir = new File(path.toOSString());
        if (! targetDir.isDirectory())
            return null;

        for (File file : targetDir.listFiles()) {
            if (! file.getName().endsWith(JAR))
                continue;

            // Ignore sources jar
            if (file.getName().contains(SOURCES))
                continue;

            if (file.getName().startsWith(pluginId))
                return file.getAbsolutePath();
        }

        return null;
    }

    /**
     * The location of this plugin from the filesystem
     *
     * @return path of plugin
     */
    public String findPlugin(String pluginId) {
        IPath path = PluginResourceLocator.getPluginRootPath(pluginId);
        String jarPath = findPluginJar(path);

        if (jarPath == null) {
            /*
             * Should normally exist in installed environment but when developing it will not.
             * Use the backup version of checking the target directory for a build jar of this plugin.
             */
            jarPath = findTargetJar(path, pluginId);
        }

        return jarPath;
    }

}
